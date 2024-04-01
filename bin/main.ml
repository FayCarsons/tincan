module Server : Riot.Application.Intf = Chat.Server.Handler
module TUI : Riot.Application.Intf = Chat.Tui

let () =
  let open Riot in
  Runtime.set_log_level None;
  (* This calls the `start` fn in each module and waits until all procsses have terminated *)
  start ~apps:[ (module Logger); (module Server); (module TUI) ] ()
