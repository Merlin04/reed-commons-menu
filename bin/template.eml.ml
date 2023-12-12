open Scraper
open Timmy
open Dtime

let debug_include_data_in_response = false

let class_name_of_item_flag f =
  show_item_flag f |> String.lowercase_ascii

let display_name_of_item_flag = function
  | FarmToFork -> "farm to fork"
  | SeafoodWatch -> "seafood watch"
  | f -> class_name_of_item_flag f

let render_item_flag f =
  <span class="flag <%s class_name_of_item_flag f %>"><%s display_name_of_item_flag f %></span>

let render_station (s : station) =
  let { label; items = station_items; _ } = s in
  let { items; _ } = !Scraper.data |> Option.get in
  <div class="station">
  <h3><%s label %></h3>
  <ul>
% station_items |> List.iter begin fun item_id -> let { label; description; price; flags; _ } = List.find (fun ({ id; _ } : menu_item) -> id = item_id) items in
    <li><h4><%s label %>
% if price <> "" then begin
          <span class="price">(<%s price %>)</span>
% end
% else ();
% flags |> List.iter begin fun f ->
        <%s! render_item_flag f %>
% end;
    </h4>
% if description <> "" then begin
      <span class="description"><%s description %></span>
% end
% else ();
    </li>
% end;
  </ul>
  </div>

let render_daypart d =
  let { label; starttime; endtime; stations; message; _ } = d in
  <div class="daypart">
  <h2><%s label %> (open <%s string_of_time starttime %> to <%s string_of_time endtime %>)</h2>
%if message <> "" then begin
  <p class="alert"><%s message %></p>
% end
% else ();
  <div class="stations">
% stations |> List.filter (fun (station : station) -> ["1"; "3"; "4"] |> List.exists (fun p -> String.starts_with ~prefix:p station.id) |> not) |> List.iter begin fun s ->
  <%s! render_station s %>
% end;
  </div>
  </div>

let render_data () =
  let { items = _items; dayparts } = (!Scraper.data) |> Option.get in
  let now = now () in
  print_endline (string_of_time now);
  (* debug end/start times *)
  print_endline (string_of_time (List.hd dayparts).endtime);
  print_endline (string_of_time (List.hd dayparts).starttime);
  let (past, present, future) = List.fold_left (fun (a, p, f) cur ->
    if Daytime.compare cur.endtime now = -1 then (cur :: a, p, f) else
    if Daytime.compare cur.starttime now = 1 then (a, p, cur :: f)
    else (a, cur :: p, f)) ([], [], []) dayparts in
  <div id="contents">
% if past <> [] then begin
  <details>
    <summary>past hours (<%s past |> List.map (fun d -> d.label) |> String.concat ", " %>) </summary>
    <div>
% past |> List.iter begin fun d ->
      <%s! render_daypart d %>
% end;
    </div>
  </details>
% end
% else ();
% if future <> [] then begin
  <details>
    <summary>future hours (<%s future |> List.map (fun d -> d.label) |> String.concat ", " %>) </summary>
    <div>
% future |> List.iter begin fun d ->
      <%s! render_daypart d %>
% end;
    </div>
  </details>
% end
% else ();
% if List.length present <> 0 then begin
% present |> List.iter begin fun d ->
  <%s! render_daypart d %>
% end;
% end
% else begin
  <p class="alert">
    commons isn't open right now :(
% if List.length past <> 0 then begin
    <br /><%s (List.nth past 0).label %> closed at <%s string_of_time (List.nth past 0).endtime %>
% end
% else ();
% if List.length future <> 0 then begin
    <br /><%s (List.nth future 0).label %> will open at <%s string_of_time (List.nth future 0).starttime %>
% end
% else ();
  </p>
% end;
  </div>

let render () =
  <!DOCTYPE html>
  <html>
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <!--<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/water.css@2/out/light.css" />-->
    <link rel="stylesheet" href="/assets/app.css" />
    <title>Reed Commons Menu</title>
% if !Scraper.data |> Option.is_none then begin
    <meta http-equiv="refresh" content="1" />
% end
% else ();
  </head>
  <body>
    <h1 id="title">Reed Commons Menu</h1>
% if !Scraper.data |> Option.is_some then begin
    <%s! render_data () %>
% if debug_include_data_in_response then begin
    <%s Scraper.show (!Scraper.data |> Option.get) %>
% end
% else ();
% end
% else ();
    <p id="footer">Up to date as of <%s Timmy.Time.to_string ~timezone:Dtime.timezone !Scraper.last_updated %> <img src="https://cdn.blahaj.social/cache/custom_emojis/images/000/017/751/original/b5a9143029c6b21a.png" alt="blahaj" height=16 width=16 /></p>
  </body>
  </html>