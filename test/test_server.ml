open OUnitTest
module Server = Chat.Server
module Utils = Chat.Utils

let create_server _ =
  let open Riot in
  let server () =
    spawn_link
    @@ fun () ->
    let init = Server.init_server 8080 in
    assert (Result.is_ok init)
  in
  let client () =
    spawn_link
    @@ fun () ->
    let init = Server.init_client (Uri.of_string "//0.0.0.0:8080") in
    assert (Result.is_ok init)
  in
  let test () =
    let server = server () in
    let client = client () in
    send server Utils.Close;
    send client Utils.Close
  in
  run test
;;

let server_suite = "TCP server tests" >::: [ "Creating a server" >:: create_server ]
let () = OUnit2.run_test_tt_main server_suite
