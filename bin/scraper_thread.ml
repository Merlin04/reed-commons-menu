open Ppx_deriving_yojson_runtime
open Lwt.Syntax
open Cohttp_lwt_unix
open Logs_lwt
open Scraper

(* i think this should be good enough? if we need to, generalize to just 262 or add allowlist for 265, 266 *)
let item_id_denylist = ["2627"; "2628"]

let filter_station_items : string list -> string list = List.filter (fun v ->
  not @@ List.exists (fun p -> String.starts_with ~prefix:p v) item_id_denylist
)

let menu_items_re = Re.Perl.re "Bamco\\.menu\\_items = (.*);" |> Re.Perl.compile
let daypart_re = Re.Perl.re "Bamco\\.dayparts\\['(\\d+)'\\] = (.*);" |> Re.Perl.compile

let debug_str s = print_endline s; s

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
      | Error s -> Logs.err (fun f -> f "Failed to parse daypart JSON: %s" s); None)
    |> List.fold_left (fun ao co -> let@ acc = ao in let@ cur = co in Some (cur :: acc)) (Some [])
    |> Option.map (fun ds -> ds
        |> List.map (fun d -> { d with stations =
            List.map (fun (s : station) -> { s with items = filter_station_items s.items }) d.stations
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

let run () =
  let rec loop () =
    let* () = Lwt.catch (fun () ->
      let* () = Lwt.catch (fun () ->
        let* redis = Redis_lwt.Client.connection_spec Constants.redis_host |> Redis_lwt.Client.connect in
        let+ m = Redis_lwt.Client.get redis Constants.message_key in
        Mutable_state.(set_state_field Fields.message m)
      ) (fun e ->
        Mutable_state.(set_state_field Fields.message None);
        warn (fun f -> f "Failed to get message from Redis: %s" (Printexc.to_string e))
      ) in
      let* () = update_data () in
      let* () = info (fun f -> f "Updated data!") in
      Lwt_unix.sleep 60.
    ) (fun e -> err (fun f -> f "Uncaught exception in scraper: %s" (Printexc.to_string e))) in
    loop ()
  in loop ()