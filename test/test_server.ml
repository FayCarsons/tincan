(*
   let sender_name = Server.SocketWriter.name
   let listener_name = Server.SocketReader.name

   module TuiSim = struct
   let name = "TUI"
   let send_to_handler msg = send_by_name ~name msg

   let start () =
   let pid = spawn_link (fun () -> ()) in
   register name pid;
   Ok pid
   ;;
   end
*)
