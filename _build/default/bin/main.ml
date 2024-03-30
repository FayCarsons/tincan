let () =
  let open Riot in
  Runtime.set_log_level (Some Logger.Error);
  start
    ~workers:8
    ~apps:
      [ (module Logger)
      ; (module Server.Handler : Riot.Application.Intf)
      ; (module Tui : Riot.Application.Intf)
      ]
    ()
;;
