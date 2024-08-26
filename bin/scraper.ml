open Ppx_deriving_yojson_runtime

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

 let timespan_of_string s = match String.split_on_char ':' s with
  | [h; m] -> Timedesc.Span.For_human.make_exn ~hours:(int_of_string h) ~minutes:(int_of_string m) ()
  | _ -> failwith "Invalid time string"

let timespan_of_yojson = function
  | `String s -> Ok (timespan_of_string s)
  | _ -> Error "Time field not a string"
let timespan_to_yojson = fun t -> `String (Timedesc.Span.to_string t)

type daypart = {
  starttime : Timedesc.Span.t
    [@of_yojson timespan_of_yojson]
    [@to_yojson timespan_to_yojson]
    [@printer Timedesc.Span.pp];
  endtime : Timedesc.Span.t
    [@of_yojson timespan_of_yojson]
    [@to_yojson timespan_to_yojson]
    [@printer Timedesc.Span.pp];
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