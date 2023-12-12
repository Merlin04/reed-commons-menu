open Template
open Args

let debug_web = false

let loader _root path _request =
  match Assets.read path with
    | None -> Dream.empty `Not_Found
    | Some asset -> Dream.respond asset

let passthrough_mw : Dream.middleware = fun next_handler request -> next_handler request

let run () =
  Dream.log "Running at http://localhost:%d" args.port;
  Dream.serve ~port:args.port
  @@ Dream.logger
  @@ (if debug_web then Dream_livereload.inject_script () else passthrough_mw)
  @@ (Dream.router ([
    Dream.get "/" (fun _request ->
      render () |> Dream.html);
    Dream.get "/assets/**" (Dream.static ~loader "")
  ] @ (if debug_web then [Dream_livereload.route ()] else [])))