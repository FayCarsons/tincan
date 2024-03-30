module ChooseBackend (Switch : sig
    val current_strategy : Utils.strategy
  end) : Riot.Application.Intf = struct
  module Selected =
    (val if Switch.current_strategy = Utils.SingleProcess
         then (module Server.SingleProcess : Riot.Application.Intf)
         else (module Server.MultiProcess : Riot.Application.Intf))

  include Selected
end

module ChatBackend = ChooseBackend (Utils)

let () =
  let open Riot in
  Runtime.set_log_level (Some Logger.Error);
  start ~apps:[ (module Logger); (module ChatBackend); (module Tui) ] ()
;;
