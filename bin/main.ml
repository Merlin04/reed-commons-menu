let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  (let open CalendarLib.Time_Zone in change (UTC_Plus (-8))); (* pacific time *)
  Lwt.join [
    Scraper.run ();
    Web.run ();
  ] |> Lwt_main.run;
  exit (min 1 (Logs.err_count ()))