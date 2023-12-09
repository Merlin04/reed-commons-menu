open Template
open Args

let run () =
  Dream.log "Running at http://localhost:%d" args.port;
  Dream.serve ~port:args.port
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.router [
    Dream.get "/" (fun _request ->
      render () |> Dream.html);
    Dream_livereload.route ();
  ]