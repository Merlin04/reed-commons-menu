open Template
open Args

let loader _root path _request =
  match Assets.read path with
    | None -> Dream.empty `Not_Found
    | Some asset -> Dream.respond asset

let run () =
  Dream.log "Running at http://localhost:%d" args.port;
  Dream.serve ~port:args.port
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.router [
    Dream.get "/" (fun _request ->
      render () |> Dream.html);
    Dream_livereload.route ();
    Dream.get "/assets/**" (Dream.static ~loader "")
  ]