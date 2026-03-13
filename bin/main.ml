let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Utils.lwt_both_fail_early (Scraper_thread.run ()) (Web.run ())
    |> Lwt_main.run |> ignore;
  exit (min 1 (Logs.err_count ()))
