module Server : Riot.Application.Intf = Chat.Server.Handler
module TUI : Riot.Application.Intf = Chat.Tui

let () =
  let open Riot in
  Runtime.set_log_level None;
  start ~workers:8 ~apps:[ (module Logger); (module Server); (module TUI) ] ()
;;
