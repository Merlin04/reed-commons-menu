let render () =
  <html>
  <head>
    <title>Reed Commons Menu</title>
% if !Scraper.data |> Option.is_none then begin
    <meta http-equiv="refresh" content="1">
% end
% else ();
  </head>
  <body>
    <h1>Yeah!</h1>
    this is testing ! another test
% if !Scraper.data |> Option.is_some then begin

% (!Scraper.data) |> Option.get |> Scraper.dayparts |> List.iter begin fun { Scraper.label; _} ->
    <h1><%s label %></h1>
% end;

    <%s !Scraper.data |> Option.get |> Scraper.show %>

% end
% else ();
  </body>
  </html>