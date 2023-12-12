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
    <link rel="stylesheet" href="/assets/app.css" />
    <title>Reed Commons Menu</title>
    <meta name="description" content="Daily menu for Reed College Commons cafeteria. See what food Bon Appetit's cafe is serving at the dining hall today." />
% if !Scraper.data |> Option.is_none then begin
    <meta http-equiv="refresh" content="1" />
% end
% else ();
  </head>
  <body>
    <h1 id="title">Reed Commons Cafe Menu</h1>
% if !Scraper.data |> Option.is_some then begin
    <%s! render_data () %>
% if debug_include_data_in_response then begin
    <%s Scraper.show (!Scraper.data |> Option.get) %>
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
      <li><b>Homer's Hut</b> is a convenience store shop run by Reed's non-profit <a href="https://www.reed.edu/guidebook/coll_resource/bookstore.html">bookstore</a>. It has a wide variety of snacks, meal options (including ramen), drinks, and personal care items like toothpaste, soap, birth control, condoms, ibuprofen, etc. It's inside the bookstore, which is in the GCC basement (one floor below Commons). In addition to regular daytime bookstore hours, Homer's Hut is open from 6:00 PM-2:00 AM on weekdays and 5:00 PM-2:00 AM on weekends.</li>
    </ul>
    <h3 id="site-about">Who runs this site?</h3>
    <p>Hi! I'm a Reed student/CS major - I made this site for fun (and so I could more easily see what's for dinner), and all of the source code is available <a href="https://github.com/Merlin04/bamco-menu">here</a>. If you have any questions or concerns, feel free to message me on Discord (username is the same as my GitHub username) or send me an email (address is on my <a href="https://enby.land">website</a>).</p>

    <p id="footer">Up to date as of <%s Timmy.Time.to_string ~timezone:Dtime.timezone !Scraper.last_updated %> <img src="https://cdn.blahaj.social/cache/custom_emojis/images/000/017/751/original/b5a9143029c6b21a.png" alt="blahaj" height=16 width=16 /></p>
  </body>
  </html>