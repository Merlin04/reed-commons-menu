open Scraper

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
    <h1>Reed Commons Menu</h1>
    this is testing ! another test
% if !Scraper.data |> Option.is_some then begin

% (!Scraper.data) |> Option.get |> Scraper.dayparts |> List.iter begin fun { label; starttime; endtime; stations; message; _ } ->
    <h1><%s label %> (open <%s starttime %> to <%s endtime %>)</h1>
%if message <> "" then begin
    <p class="alert"><%s message %></p>
% end
% else ();
% stations |> List.iter begin fun { label; items; _ } ->
    <h2><%s label %></h2>
    <ul>
% items |> List.iter begin fun item_id -> let { label; description; price; _ } = (!Scraper.data |> Option.get).items |> List.find (fun ({ id; _ } : menu_item) -> id = item_id) in
      <li><%s label %>
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
% end;
% end;

    <%s !Scraper.data |> Option.get |> Scraper.show %>

% end
% else ();
  </body>
  </html>