open OUnit2
module Server = Chat.Server
module Utils = Chat.Utils
module Tui = Chat.Tui

(*
   Unfortunately, due to Riot's multi-threading and scheduler system,
   testing the Server and some of the TUI
   is highly involved, and I haven't had time
   to get the coverage where I want it
*)

let test_acknowledged _ = assert (String.equal Server.Connection.acknowleged "$PING")

let server_suite =
  "TCP_server_tests" >::: [ "test acknowleged flag equality works" >:: test_acknowledged ]
;;

let () = OUnit2.run_test_tt_main server_suite

(** tests both the update fn output for a given idx and the `bound_idx` function used to
    restrict indices to valid menu options *)
let test_menu_idx _ =
  let open Minttea in
  let open Tui in
  let initial_state = Menu 0 in
  assert_equal
    (update (Event.KeyDown (Event.Down, No_modifier)) initial_state)
    (Menu 1, Command.Noop);
  assert_equal
    (update (Event.KeyDown (Event.Up, No_modifier)) initial_state)
    (Menu 1, Command.Noop);
  assert_equal
    (update (Event.KeyDown (Event.Down, No_modifier)) (Menu 1))
    (Menu 0, Command.Noop)
;;

(* Test TUI's responses to messages sent server->tui *)
let test_custom_events _ =
  let open Minttea in
  let open Tui in
  let init_state =
    { role = Host
    ; connection = Open Riot.Pid.zero
    ; acknowleged = HostAcknowleged
    ; text = Leaves.Text_input.empty ()
    ; spinner = None
    ; history = []
    }
  in
  assert_equal
    (update (Event.Frame Ptime.min) (Chat init_state))
    (Chat init_state, Command.Noop);
  assert_equal
    (update (Event.Custom (Utils.Err "oops")) (Chat init_state))
    (Chat { init_state with history = [ init_state.role, "oops" ] }, Command.Noop);
  assert_equal
    (update (Event.Custom (Utils.Received "hi")) (Chat init_state))
    (Chat { init_state with history = [ not_role init_state.role, "hi" ] }, Command.Noop);
  assert_equal
    (update (Event.Custom Utils.Acknowleged) (Chat init_state))
    (Chat init_state, Command.Noop);
  assert_equal
    (update
       (Event.Custom (Utils.Connected Riot.Pid.zero))
       (Chat { init_state with connection = Waiting }))
    ( Chat { init_state with history = [ Host, "Established connection with client" ] }
    , Command.Noop );
  assert_equal
    (update (Event.Custom Utils.Closed) (Chat init_state))
    (Chat { init_state with connection = Shutdown }, Command.Noop)
;;

(* Test key down events, not full coverage as some require a server process to be running *)
let test_key_events _ =
  let open Minttea in
  let open Tui in
  let init_state =
    { role = Host
    ; connection = Waiting
    ; acknowleged = HostAcknowleged
    ; text = Leaves.Text_input.empty ()
    ; spinner = None
    ; history = []
    }
  in
  assert_equal
    (update (Event.KeyDown (Event.Escape, No_modifier)) (Chat init_state))
    (Menu 0, Command.Hide_cursor);
  let with_char, _ =
    update (Event.KeyDown (Event.Key "a", No_modifier)) (Chat init_state)
  in
  match with_char with
  | Chat { text; _ } -> assert_equal (Leaves.Text_input.current_text text) "a"
  | _ -> failwith "Char not added to text buffer"
;;

(* Tests spinner updates that happen in response Event.Frame events *)
let test_frame _ =
  let open Minttea in
  let open Tui in
  let init_state =
    { role = Host
    ; connection = Open Riot.Pid.zero
    ; acknowleged = HostAcknowleged
    ; text = Leaves.Text_input.empty ()
    ; spinner = None
    ; history = []
    }
  in
  let updated, _ = update (Event.Frame Ptime.min) (Chat init_state) in
  (match updated with
   | Chat { spinner; _ } -> assert (Option.is_none spinner)
   | _ -> failwith "Spinner created during update");
  let with_spinner = { init_state with spinner = Some Leaves.Spinner.dot } in
  let updated, _ = update (Event.Frame Ptime.min) (Chat with_spinner) in
  let original, updated =
    match with_spinner, updated with
    | og, Chat updated when Option.is_some og.spinner && Option.is_some updated.spinner ->
      Option.get og.spinner, Option.get updated.spinner
    | _ -> failwith "Spinners nott properly updated"
  in
  assert_bool "Spinner changed pattern during update" (original == updated)
;;

let tui_suite =
  "Terminal_UI_tests"
  >::: [ "Test menu updates" >:: test_menu_idx
       ; "Test chat upddate" >:: test_custom_events
       ; "Test KeyDown events" >:: test_key_events
       ; "Test Frame update" >:: test_frame
       ]
;;

let () = OUnit2.run_test_tt_main tui_suite
