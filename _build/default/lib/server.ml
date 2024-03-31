open Riot
open Utils
module Socket = Net.Socket
module Addr = Net.Addr
module TcpListener = Net.Tcp_listener
module TcpStream = Net.Tcp_stream

let ( let* ) = Result.bind

(** [string_of_error IO.io_error] converts an io_error to a string *)
let string_of_error = function
  | `Would_block -> "Would block!"
  | `No_info -> "No info\r\n"
  | `Connection_closed -> Printf.sprintf "Connection closed\r\n%!"
  | `Exn exn -> Printf.sprintf "Exn: %S\r\n%!" (Printexc.to_string exn)
  | `Unix_error err -> Printf.sprintf "Unix error: %S\r\n%!" (Unix.error_message err)
  | `Noop -> "NoOp\r\n"
  | `Eof -> "End of file\r\n"
  | `Closed -> "Closed\r\n"
  | `Process_down -> "Process down!\r\n"
  | _ -> Printf.sprintf "other error\r\n"
;;

(** [send_err IO.io_error] sends an error to the TUI *)
let send_err recv err_msg = send recv @@ Err (string_of_error err_msg)

module TcpServer = struct
  open Logger.Make (struct
      let namespace = [ "tincan"; "single-process handler" ]
    end)

  type t =
    { reader : TcpStream.t IO.Reader.t
    ; writer : TcpStream.t IO.Writer.t
    ; conn : TcpStream.t
    ; role : role
    ; recv : Pid.t
    }

  let name = "Chat.handler"

  let rec send_message ({ writer; recv; _ } as state) buf =
    let bufs = IO.Iovec.from_string @@ buf ^ "\r\n" in
    Logger.error (fun f -> f "Sending messsage: %s" buf);
    match IO.write_all_vectored writer ~bufs with
    | Ok _ ->
      (match IO.flush writer with
       | Ok () -> Logger.error (fun f -> f "Flushed writer!")
       | exception e -> send recv @@ Err (Printexc.to_string e)
       | Error err -> send_err recv err)
    | exception e ->
      let reason = Printexc.to_string e in
      Logger.error (fun f -> f "EXCEPTION: %s" reason);
      send recv @@ Err reason
    | Error `Would_block | Error `Timeout ->
      yield ();
      send_message state buf
    | Error err ->
      Logger.error (fun f -> f "ERROR SENDING: %a" IO.pp_err err);
      send_err recv err
  ;;

  let read ({ recv; role; reader; _ } as state) =
    match
      Bytestring.with_bytes ~capacity:1024 @@ fun buf -> IO.read ~timeout:10L reader buf
    with
    | Ok bs when Bytestring.length bs = 0 -> send recv Closed
    | Ok msg when role = Host ->
      Logger.error (fun f -> f "RECEIVED `%a` OVER TCP" Bytestring.pp msg);
      let msg = Bytestring.to_string msg in
      send recv @@ Received msg;
      send_message state acknowleged
    | Ok msg ->
      Logger.error (fun f -> f "RECEIVED `%a` OVER TCP" Bytestring.pp msg);
      let raw = Bytestring.to_string msg |> String.trim in
      let is_ping = String.starts_with ~prefix:acknowleged raw in
      Logger.error (fun f -> f "RECEIVED IS PING? %b" is_ping);
      let msg_to_tui = if is_ping then Acknowleged else Received raw in
      send recv msg_to_tui
    | Error `Would_block | (exception Syscall_timeout) -> ()
    | Error err ->
      send_err recv err;
      shutdown ()
  ;;

  let start_loop state =
    let rec loop ({ conn; recv; _ } as state) =
      match receive_any ~after:10L () with
      | exception Receive_timeout ->
        read state;
        loop state
      | exception e -> send recv @@ Err (Printexc.to_string e)
      | Send buf ->
        send_message state buf;
        loop state
      | Close ->
        TcpStream.close conn;
        shutdown ()
      | _ -> loop state
    in
    loop state
  ;;

  let start recv role conn =
    let reader = TcpStream.to_reader conn in
    let writer = TcpStream.to_writer conn in
    let pid = spawn_link @@ fun () -> start_loop { recv; reader; writer; conn; role } in
    register name pid;
    send recv (Connected pid)
  ;;
end

(** [init_server int state] creates a TCP socket and begins listening for connections on it
    Once a connection is established it sends a "Connected" message to the TUI
    and returns state *)
let init_server port =
  let* socket = TcpListener.bind ~port () in
  Logger.error (fun f -> f "Created server on port %d" port);
  let* conn, addr = TcpListener.accept socket in
  Logger.error (fun f -> f "Connected with %a" Addr.pp addr);
  Ok (Host, conn)
;;

(** [init_client uri] connects socket to URI,
    Sends "Connected" message to TUI
    and returns state *)
let init_client uri =
  let* sockaddr = Addr.of_uri uri in
  let* conn = TcpStream.connect sockaddr in
  Ok (Client, conn)
;;

module Handler = struct
  open Logger.Make (struct
      let namespace = [ "tincan"; "single-process supervisor" ]
    end)

  let name = "Chat"

  (** [handler unit] Listens for initialization or close messages from TUI
      From there it initializes the TCP connection and child processes with
      the correct state *)
  let handler () =
    let rec loop () =
      match receive_any () with
      (* User is starting in host mode *)
      | StartServer (port, recv) ->
        Logger.error (fun f -> f "RECEIVED `Startserver` FROM TUI");
        (match init_server port with
         | Ok (role, conn) ->
           TcpServer.start recv role conn;
           loop ()
         | Error err ->
           send recv @@ Err (string_of_error err);
           loop ())
      (* User is starting in client mode *)
      | StartClient (uri, recv) ->
        Logger.error (fun f -> f "RECEIVED `Startserver` FROM TUI");
        (match init_client uri with
         | Ok (role, conn) ->
           TcpServer.start recv role conn;
           loop ()
         | Error err ->
           send recv @@ Err (string_of_error err);
           loop ())
      (* User has quit app *)
      | Close ->
        Logger.error (fun f -> f "RECEIVED `Close` FROM TUI");
        shutdown ()
      (* Unkonwn/irrelevant message*)
      | _ -> loop ()
    in
    loop ()
  ;;

  (** [start unit] begins {handler} and registers "Chat" to its process ID*)
  let start () =
    Logger.set_log_level None;
    let pid = spawn_link handler in
    register chat_name pid;
    Ok pid
  ;;
end
