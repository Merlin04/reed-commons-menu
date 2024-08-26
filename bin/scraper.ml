open Ppx_deriving_yojson_runtime
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

type t = {
  items : menu_item list;
  dayparts : daypart list;
}
[@@deriving show]