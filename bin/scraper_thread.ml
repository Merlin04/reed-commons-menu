open Ppx_deriving_yojson_runtime
open Lwt.Syntax
open Cohttp_lwt_unix
open Logs_lwt
open Scraper

let menu_items_re = Re.Perl.re "Bamco\\.menu\\_items = (.*);" |> Re.Perl.compile
let daypart_re = Re.Perl.re "Bamco\\.dayparts\\['(\\d+)'\\] = (.*);" |> Re.Perl.compile

let debug_str s = print_endline s; s

(* 2025-10-09: for some reason, bamco decided to change it so a station now represents an entire cafe, and there's
   additional data attached to the menu_item telling you what the actual station is *)
(* 2025-10-27: they reverted the change?? but kept all the new fields?? this code still works so i'll just keep it
   around in case they bring the change back *)
let transform_stations (menu_items : menu_item list) (stations : station list) =
  stations
    |> List.concat_map (fun (s : station) -> s.items)
    |> List.map (fun id -> menu_items |> List.find (fun (i : menu_item) -> id = i.id)) (* get menu_items *)
    |> List.filter (fun item -> item.tier = 1)
    |> List.map (fun item -> (item.station, item))
    |> List.fold_left (fun acc (k, v) ->
          match (List.assoc_opt k acc) with
            | Some l -> (k, (v :: l)) :: (List.remove_assoc k acc)
            | None -> (k, [v]) :: acc
      ) []
    |> List.rev
    |> List.map (fun (s, is) -> { id = ""; label = s; items = is |> List.map (fun (i : menu_item) -> i.id) |> List.rev })

let parse_doc body : t option =
  let ( let@ ) = Option.bind in
  let@ items = (let m = Re.all menu_items_re body in Re.Group.get (List.nth m 0) 1)
    |> Yojson.Safe.from_string |> menu_items_of_yojson |> function
      | Ok v -> Some v
      | Error s -> Logs.err (fun f -> f "Failed to parse menu items JSON: %s" s); None
  in let@ dayparts = Re.all daypart_re body
    |> List.map (fun g -> (Re.Group.get g 1 |> int_of_string, Re.Group.get g 2))
    |> List.sort (fun (n1, _) (n2, _) -> n1 - n2)
    |> List.map (fun (_, p) -> p |> Yojson.Safe.from_string |> daypart_of_yojson |> function
      | Ok v -> Some v
      | Error s -> Logs.err (fun f -> f "Failed to parse daypart JSON: %s\n%s\n\n" s p); None)
    |> List.fold_left (fun ao co -> let@ acc = ao in let@ cur = co in Some (cur :: acc)) (Some [])
    |> Option.map (fun ds -> ds
        |> List.map (fun d -> { d with stations =
            transform_stations items d.stations
              |> List.filter (fun (s : station) -> List.length s.items <> 0) })
        |> List.rev
    )
  in Some { items; dayparts }

let fetch_body () : (string, int) result Lwt.t =
  let open Cohttp in
  let* (resp, body) = Client.get (Uri.of_string Constants.ba_url) in
  let code = resp |> Response.status |> Code.code_of_status in
  if code <> 200 then (Error code) |> Lwt.return else
  let+ b = body |> Cohttp_lwt.Body.to_string in
  Ok b

module Mutable_state = struct
  type t = {
    mutable data : Scraper.t option;
    mutable last_updated : Timedesc.Timestamp.t;
    mutable stale : bool;
    mutable message : string option;
  }
  [@@deriving fields ~fields]

  (* todo: maybe some day learn how to use metapp (https://github.com/thierry-martinez/metapp) and use that to generate this? *)
  (* definitely overkill but it would be cool *)
  type t_immutable = {
    data : Scraper.t option;
    last_updated : Timedesc.Timestamp.t;
    stale : bool;
    message : string option;
  }
  [@@deriving stable_record ~version:t]

  let _state : t = {
    data = None;
    last_updated = (Timedesc.Timestamp.now ());
    stale = true;
    message = None;
  }

  let update_since_last_use = ref true

  let use () =
    update_since_last_use := false;
    t_immutable_of_t _state

  let set_state_field (f : ([> `Set_and_create ], 'r, 'a) Fieldslib.Field.t_with_perm) (value : 'a) =
    if Fieldslib.Field.get f _state != value then (
        (Fieldslib.Field.setter f |> Option.get) _state value;
        update_since_last_use := true
    ) else ()
end

let update_data () =
  let* body = fetch_body () in
  match body with
    | Ok body -> (match parse_doc body with
      | Some v ->
        let open Mutable_state in
        set_state_field Fields.data (Some v);
        set_state_field Fields.last_updated (Timedesc.Timestamp.now ());
        set_state_field Fields.stale false;
        Lwt.return_unit
      | None ->
        let open Mutable_state in
        set_state_field Fields.stale true;
        warn (fun f -> f "Parsing doc failed; not updating cached value"))
    | Error code -> err (fun f -> code |> string_of_int |> f "Failed to fetch doc: %s")

let rec run () =
  let* () = Lwt.catch (fun () ->
    (* get data from redis *)
    let* () = try (
      let redis = Redis_sync.Client.connection_spec Constants.redis_host |> Redis_sync.Client.connect in
      let m = Redis_sync.Client.get redis Constants.message_key in
      Redis_sync.Client.disconnect redis;
      Mutable_state.(set_state_field Fields.message m); Lwt.return_unit
    ) with e -> (
      Mutable_state.(set_state_field Fields.message None);
      warn (fun f -> f "Failed to get message from Redis: %s" (Printexc.to_string e))
    ) in
    (* get data from ba_url *)
    let* () = update_data () in
    info (fun f -> f "Updated data!")
  ) (fun e -> err (fun f -> f "Uncaught exception in scraper: %s" (Printexc.to_string e))) in
  let* () = Lwt_unix.sleep 60. in
  run ()
