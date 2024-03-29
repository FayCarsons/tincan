let () =
  let open Riot in
  Runtime.set_log_level (Some Logger.Error);
  start ~workers:8 ~apps:[ (module Riot.Logger); (module Server); (module Tui) ] ()
;;
