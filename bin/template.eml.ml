let debug_include_data_in_response = false

let escaped_br_re = Re.Perl.re "&lt;br( )*/&gt;" |> Re.Perl.compile
let escape_html_except_br str = Dream.html_escape str |> Re.replace_string escaped_br_re ~by:"<br />"

let tz = Timedesc.Time_zone.make_exn "America/Los_Angeles"

let string_of_timespan (s : Timedesc.Span.t) =
  let v = Timedesc.Span.For_human.view s in
  let h = v.hours mod 12 in
  let h = if h = 0 then 12 else h in
  let pm_str = if v.hours / 12 > 0 then "PM" else "AM" in
  if v.minutes <> 0 then
    (string_of_int h) ^ ":" ^ (string_of_int v.minutes) ^ pm_str
  else (string_of_int h) ^ pm_str

let asset name =
  let hash = match Assets.hash name with
    | Some h -> h
    | None -> failwith @@ "Asset " ^ name ^ " does not exist"
  in
  "/assets/" ^ name ^ "?" ^ hash

let class_name_of_item_flag f =
  Scraper.show_item_flag f |> String.lowercase_ascii

let display_name_of_item_flag = let open Scraper in function
  | FarmToFork -> "farm to fork"
  | SeafoodWatch -> "seafood watch"
  | f -> class_name_of_item_flag f

let render_item_flag f =
  <span class="flag <%s class_name_of_item_flag f %>"><%s display_name_of_item_flag f %></span>

let render_station (s : Scraper.station) (menu_items : Scraper.menu_item list) =
  let open Scraper in
  let { label; items = station_items; _ } = s in
  let get_item_by_id item_id = List.find (fun ({ id; _ } : menu_item) -> id = item_id) menu_items in
  <div class="station">
  <h3><%s label %></h3>
  <ul>
% station_items |> List.iter begin fun id -> let { label; description; price; flags; _ } = get_item_by_id id in
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
      <span class="description"><%s! (escape_html_except_br description) %></span>
% end
% else ();
    </li>
% end;
  </ul>
  </div>

let render_daypart d menu_items =
  let open Scraper in
  let { label; starttime; endtime; stations; message; _ } = d in
  <div class="daypart">
  <h2><%s label %> (open <%s string_of_timespan starttime %> to <%s string_of_timespan endtime %>)</h2>
%if message <> "" then begin
  <p class="alert"><%s message %></p>
% end
% else ();
  <div class="stations">
% stations |> List.filter (fun (station : station) -> ["1"; "3"; "4"] |> List.exists (fun p -> String.starts_with ~prefix:p station.id) |> not) |> List.iter begin fun s ->
  <%s! render_station s menu_items %>
% end;
  </div>
  </div>

let render_data ({ items; dayparts } : Scraper.t) (stale : bool) =
  let now = Timedesc.now ~tz_of_date_time:tz () |> Timedesc.time |> Timedesc.Time.to_span in
(*  print_endline (string_of_timespan now); *)
  (* debug end/start times *)
(*  print_endline (string_of_timespan (List.hd dayparts).endtime); *)
(*  print_endline (string_of_timespan (List.hd dayparts).starttime); *)
  let (past, present, future) = List.fold_left (fun (a, p, f) cur ->
    if Timedesc.Span.compare cur.Scraper.endtime now = -1 then (cur :: a, p, f) else
    if Timedesc.Span.compare cur.Scraper.starttime now = 1 then (a, p, cur :: f)
    else (a, cur :: p, f)) ([], [], []) dayparts in
  let future = List.rev future in
  <div id="contents">
% if stale then begin
  <p class="alert">
    There was an internal error fetching the most up-to-date menu data from Bon Appetit; if this continues for more than a minute or two and the <a href="https://reed.cafebonappetit.com">official menu site</a> is working properly, please <a href="site-about">contact me</a> because something is broken.
  </p>
% end
% else ();
% if past <> [] then begin
  <details>
    <summary>past hours (<%s past |> List.map (fun d -> d.Scraper.label) |> String.concat ", " %>) </summary>
    <div>
% past |> List.iter begin fun d ->
      <%s! render_daypart d items %>
% end;
    </div>
  </details>
% end
% else ();
% if future <> [] then begin
  <details>
    <summary>future hours (<%s future |> List.map (fun d -> d.Scraper.label) |> String.concat ", " %>) </summary>
    <div>
% future |> List.iter begin fun d ->
      <%s! render_daypart d items %>
% end;
    </div>
  </details>
% end
% else ();
% if List.length present <> 0 then begin
% present |> List.iter begin fun d ->
  <%s! render_daypart d items %>
% end;
% end
% else begin
  <p class="alert">
    commons isn't open right now :(
% if List.length past <> 0 then begin
    <br /><%s (List.nth past 0).label %> closed at <%s string_of_timespan (List.nth past 0).endtime %>
% end
% else ();
% if List.length future <> 0 then begin
    <br /><%s (List.nth future 0).label %> will open at <%s string_of_timespan (List.nth future 0).starttime %>
% end
% else ();
  </p>
% end;
  </div>

let render ~data ~stale ~last_updated ~message () =
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <link rel="stylesheet" href="<%s asset "app.css" %>" />
    <title>Reed Commons Menu</title>
    <meta name="description" content="Daily menu for Reed College Commons cafeteria. See what food Bon Appetit's cafe is serving at the dining hall today." />
% if Option.is_none data then begin
    <meta http-equiv="refresh" content="1" />
% end
% else ();
    <!-- Google tag (gtag.js) -->
    <script async src="https://www.googletagmanager.com/gtag/js?id=G-1631GPJYKP"></script>
    <script>
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());

      gtag('config', 'G-1631GPJYKP');
    </script>
  </head>
  <body>
    <h1 id="title">Reed Commons Cafe Menu</h1>
% if Option.is_some message then begin
    <div class="info">
      <%s! Option.get message %>
    </div>
% end
% else ();
% if Option.is_some data then begin
    <%s! render_data (Option.get data) stale %>
% if debug_include_data_in_response then begin
    <%s data |> Option.get |> Scraper.show %>
% end
% else ();
% end
% else ();
    <h2 id="faqs">FAQs</h2>
    <p><a href="https://www.reed.edu/res_life/dining-food-services/bon-appetit.html">Reed College's Commons cafeteria</a> is the main cafe and only full dining hall on campus, located in the Gray Campus Center (GCC), with a variety of stations including vegetarian, vegan, halal, and made without gluten options. It is run by Bon Appetit food services company. Commons takes board points, cash, and card. The dining hall also has microwaves.</p>
    <h3 id="commons-hours">What are Reed Commons hours?</h3>
    <p>Hours for Commons vary, but typically on weekdays they are open from 7:30 AM-10:30 AM for breakfast, 11:00 AM-4:00 PM for lunch, and 5:00 PM-7:00 PM for dinner. On weekends, they are usually open from 10:00 AM-2:30 PM for brunch and 5:00 PM-7:00 PM for dinner. Be sure to check this site to see if Commons is open right now before going.</p>
    <h3 id="commons-address">Where is Reed College dining hall located?</h3>
    <p>Reed's Commons dining hall is located in the <a href="https://maps.app.goo.gl/jaMJVc3RVkges2SF7">Gray Campus Center (GCC)</a>, behind the Old Dorm Block and near the Performing Arts Building (PAB). Its address is the same as Reed's address, which is 3203 SE Woodstock Blvd, Portland, OR 97202.</p>
    <h3 id="campus-meal-options">Where can I eat at Reed College?</h3>
    In addition to Commons, there are a few other places to eat on campus:
    <ul>
      <li><b>Paradox Cafe</b> is an independent student-run cafe with two on-campus locations; the Olde Shoppe is located inside the Student Union (attached to the GCC) and the Nu Shoppe (also called Paradox Lost) is attached to the biology building. Paradox offers (in my opinion, very good) drinks and pastries and a great place to hang out or study. If you're able to, go to Paradox instead of one of Bon Appetit's cafes to support student workers and <a href="https://reedquest.org/2023/10/14/paradox-regained/">keep Paradox alive!</a></li>
      <li><b>Canyon Cafe</b> (run by Bon Appetit) is located across the canyon near the Grove and offers a variety of drinks and snacks. Canyon Cafe is usually open from 7:30 AM-3:00 PM on weekdays.</li>
      <li><b>Marketplace</b> (also run by Bon Appetit) is inside Commons and offers various prepackaged snacks and drinks, as well as some pantry essentials. It's usually open from 10:00 AM-9:00 PM on the weekdays and 1:00 PM-5:00 PM on the weekends.</li>
      <li><b>Homer's Hut</b> is a convenience store shop run by <a href="https://www.reed.edu/guidebook/coll_resource/bookstore.html">Reed's non-profit bookstore</a>. It has a wide variety of snacks, meal options (including ramen), drinks, and personal care items like toothpaste, soap, birth control, condoms, ibuprofen, etc. It's inside the bookstore, which is in the GCC basement (one floor below Commons). In addition to regular daytime bookstore hours, Homer's Hut is open from 6:00 PM-2:00 AM on weekdays and 5:00 PM-2:00 AM on weekends.</li>
    </ul>
    <h3 id="site-about">Who runs this site?</h3>
    <p>Hi! I'm a Reed student/CS major - I made this site for fun (and so I could more easily see what's for dinner) (and to procrastinate starting my finals), and you can check out the <a href="https://github.com/Merlin04/reed-commons-menu">source code on GitHub</a>. If you have any questions or concerns, feel free to message me on Discord (username is the same as my GitHub username) or send me an email (address is on <a href="https://enby.land">my website</a>).</p>

    <p id="footer">Up to date as of <%s Timedesc.Timestamp.to_string ~display_using_tz:tz last_updated %> <img src="<%s asset "blahaj.png" %>" alt="blahaj" height=16 width=16 /></p>
  </body>
  </html>