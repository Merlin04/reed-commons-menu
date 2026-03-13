(* this function taken from
   https://gitlab.com/nomadic-labs/tezt/-/blob/2b2772ab8d63f9552c17de51b1f09b01311ffa94/lib_core/base.ml#L44,
   MIT license, Copyright (c) 2018-2022 Nomadic Labs, Copyright (c) 2020 Metastate AG *)
let lwt_both_fail_early a b =
  let main_promise, main_awakener = Lwt.task () in
  let already_woke_up = ref false in
  Lwt.on_failure a (fun exn ->
      if not !already_woke_up then (
        already_woke_up := true ;
        Lwt.wakeup_exn main_awakener exn) ;
      Lwt.cancel b) ;
  Lwt.on_failure b (fun exn ->
      if not !already_woke_up then (
        already_woke_up := true ;
        Lwt.wakeup_exn main_awakener exn) ;
      Lwt.cancel a) ;
  let both = Lwt.both a b in
  Lwt.on_success both (fun x -> Lwt.wakeup main_awakener x) ;
  Lwt.on_cancel main_promise (fun () -> Lwt.cancel both) ;
  main_promise
