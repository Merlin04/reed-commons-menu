open Timmy

let timezone = Timezone.of_gmt_offset_seconds (-8 * 60 * 60) (* PST *)

(* do this manually; timmy doesn't provide it *)
(* it's just in the format HH:MM *)
let time_of_string s = match String.split_on_char ':' s with
  | [h; m] -> Daytime.make ~hours:(int_of_string h) ~minutes:(int_of_string m) ~seconds:0 |> Stdlib.Result.get_ok
  | _ -> failwith "Invalid time string"

let now () = Clock.now () |> Daytime.of_time ~timezone
let time_formatter = Timmy.Daytime.pp_opt ~format:`_12 ~precision:`Minutes ()
let string_of_time = Fmt.to_to_string time_formatter
let time_pp = fun fmt t -> time_formatter fmt t