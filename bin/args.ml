type config = {
  port : int;
}

let args = (
  let port_ref = ref 8080 in
  let speclist = [
    ("-p", Arg.Int (fun v -> port_ref := v), "  Port to run web server on (default: 8080)")
  ] in
  let usage_msg = "menu [-p port]" in
  Arg.parse speclist (fun _ -> ()) usage_msg;
  {
    port = !port_ref
  }
)