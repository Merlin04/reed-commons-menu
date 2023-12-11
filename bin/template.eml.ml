open Scraper
open Timmy
open Dtime

let render_station (s : station) =
  let { label; items = station_items; _ } = s in
  let { items; _ } = !Scraper.data |> Option.get in
  <h3><%s label %></h3>
  <ul>
% station_items |> List.iter begin fun item_id -> let { label; description; price; _ } = List.find (fun ({ id; _ } : menu_item) -> id = item_id) items in
    <li><b><%s label %></b>
% if description <> "" then begin
      <br /><span class="description"><%s description %></span>
% end
% else ();
% if price <> "" then begin
      <br /><span class="price"><%s price %></span>
% end
% else ();
    </li>
% end;
  </ul>

let render_daypart d =
  let { label; starttime; endtime; stations; message; _ } = d in
  <h2><%s label %> (open <%s string_of_time starttime %> to <%s string_of_time endtime %>)</h2>
%if message <> "" then begin
  <p class="alert"><%s message %></p>
% end
% else ();
% stations |> List.filter (fun (station : station) -> station.id |> String.starts_with ~prefix:"1" |> not) |> List.iter begin fun s ->
  <%s! render_station s %>
% end;

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
  <div>
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
  <p class="alert">commons isn't open right now :(</p>
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
    <!--<%s Scraper.show (!Scraper.data |> Option.get) %>-->
% end
% else ();
    <p id="footer">Up to date as of <%s Timmy.Time.to_string ~timezone:Dtime.timezone !Scraper.last_updated %></p>
  </body>
  </html>