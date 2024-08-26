open Ppx_deriving_yojson_runtime
open Lwt.Syntax
open Cohttp_lwt_unix
open Logs_lwt
open Timmy
open Dtime

type item_flag =
  | Vegan [@value 4]
  | Mwgci [@value 9]
  | Vegetarian [@value 1]
  | FarmToFork [@value 6]
  | Halal [@value 10]
  | SeafoodWatch [@value 3]
  | Humane [@value 18]
[@@deriving enum, show { with_path = false }, yojson]

type menu_item = {
  id : string;
  label : string;
  description : string;
  price : string;
  station_id : string;
  flags : item_flag list [@of_yojson (function
    | `Assoc l -> Ok (List.filter_map (fun (k, _) -> k |> int_of_string |> item_flag_of_enum) l)
    | _ -> Ok [])] [@key "cor_icon"];
}
[@@deriving show, yojson { strict = false }]

let menu_items_of_yojson : Yojson.Safe.t -> menu_item list error_or = function
  | `Assoc l -> map_bind (fun (_, v) -> menu_item_of_yojson v) [] l
  | _ -> Error "Menu items data not in expected form"

(* i think this should be good enough? if we need to, generalize to just 262 or add allowlist for 265, 266 *)
let item_id_denylist = ["2627"; "2628"]

let filter_station_items : string list -> string list = List.filter (fun v ->
  not @@ List.exists (fun p -> String.starts_with ~prefix:p v) item_id_denylist
)

type station = {
  id : string;
  label : string;
  items: string list;
}
[@@deriving show, yojson { strict = false }]
let time_of_yojson = function
  | `String s -> Ok (time_of_string s)
  | _ -> Error "Time field not a string"
let time_to_yojson = fun t -> `String (string_of_time t)
type daypart = {
  starttime : Daytime.t
    [@of_yojson time_of_yojson]
    [@to_yojson time_to_yojson]
    [@printer time_pp];
  endtime : Daytime.t
    [@of_yojson time_of_yojson]
    [@to_yojson time_to_yojson]
    [@printer time_pp];
  id : string;
  label : string;
  abbreviation : string;
  message : string;
  stations : station list;
}
[@@deriving show, yojson { strict = false }]

let menu_items_re = Re.Perl.re "Bamco\\.menu\\_items = (.*);" |> Re.Perl.compile
let daypart_re = Re.Perl.re "Bamco\\.dayparts\\['(\\d+)'\\] = (.*);" |> Re.Perl.compile

type t = {
  items : menu_item list;
  dayparts : daypart list;
}
[@@deriving show, fields ~getters]

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

let data = ref None
let last_updated = ref (Clock.now ())
let stale = ref true

let update_data () =
  let* body = fetch_body () in
  match body with
    | Ok body -> (match parse_doc body with
      | Some v -> data := Some v; last_updated := Clock.now (); stale := false; Lwt.return_unit
      | None -> stale := true; warn (fun f -> f "Parsing doc failed; not updating cached value"))
    | Error code -> err (fun f -> code |> string_of_int |> f "Failed to fetch doc: %s")

let rec run () =
  let* () = Lwt.catch (fun () ->
    let* () = update_data () in
    let* () = info (fun f -> f "Updated data!") in
    Lwt_unix.sleep 60.
  ) (fun e -> err (fun f -> f "Uncaught exception in scraper: %s" (Printexc.to_string e))) in
  run ()