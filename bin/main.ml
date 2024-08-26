let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt.join [
    Scraper_thread.run ();
    Web.run ();
  ] |> Lwt_main.run;
  exit (min 1 (Logs.err_count ()))