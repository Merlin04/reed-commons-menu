open Template
open Args
open Lwt.Syntax
open Logs_lwt

let debug_web = false

let loader _root path _request =
  match Assets.read path with
    | None -> Dream.empty `Not_Found
    | Some asset -> Dream.respond asset

let passthrough_mw : Dream.middleware = fun next_handler request -> next_handler request

let static_cached ?(duration = 31536000) request =
  let+ response = Dream.static ~loader "" request in
  Dream_pure.Message.add_header response "Cache-Control" ("max-age=" ^ string_of_int duration);
  response

let cached_html = ref ""

let run () =
  Dream.log "Running at http://localhost:%d" args.port;
  Dream.serve ~port:args.port
  @@ Dream.logger
  @@ (if debug_web then Dream_livereload.inject_script () else passthrough_mw)
  @@ (Dream.router ([
    Dream.get "/" (fun _request ->
      let open Scraper_thread.Mutable_state in
      let* () = if !update_since_last_use then (
        let { data; stale; last_updated; message } = Scraper_thread.Mutable_state.use () in
        let+ () = info (fun f -> f "Regenerating response") in
        cached_html := render ~data ~stale ~last_updated ~message ()
      ) else Lwt.return_unit in
      Dream.html !cached_html
    );
    Dream.get "/favicon.ico" @@ Dream.static ~loader "";
    Dream.get "/assets/**" static_cached
  ] @ (if debug_web then [Dream_livereload.route ()] else [])))