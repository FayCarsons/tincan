open Riot
open Utils
module Socket = Net.Socket
module Addr = Net.Addr
module TcpListener = Net.Tcp_listener
module TcpStream = Net.Tcp_stream

let ( let* ) = Result.bind

(** [assert_tui unit] for debugging, asserts that the TUI process
    is running and accessible by the expected name *)
let assert_tui () = assert (Process.where_is tui_name |> Option.is_some)

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
let send_err err_msg = send_by_name ~name:tui_name @@ Err (string_of_error err_msg)

module Handler = struct
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

  let rec send_message ({ writer; _ } as state) buf =
    let bufs = IO.Iovec.from_string @@ buf ^ "\r\n" in
    Logger.error (fun f -> f "Sending messsage: %s" buf);
    match IO.write_all_vectored writer ~bufs with
    | Ok _ ->
      (match IO.flush writer with
       | Ok () -> Logger.error (fun f -> f "Flushed writer!")
       | exception e -> send_by_name ~name:tui_name @@ Err (Printexc.to_string e)
       | Error err -> send_err err)
    | exception e ->
      let reason = Printexc.to_string e in
      Logger.error (fun f -> f "EXCEPTION: %s" reason);
      send_by_name ~name:tui_name @@ Err reason
    | Error `Would_block | Error `Timeout ->
      yield ();
      send_message state buf
    | Error err ->
      Logger.error (fun f -> f "ERROR SENDING: %a" IO.pp_err err);
      send_err err
  ;;

  let read recv role reader =
    match
      Bytestring.with_bytes ~capacity:1024 @@ fun buf -> IO.read ~timeout:100L reader buf
    with
    | Ok msg when role = Host ->
      Logger.error (fun f -> f "RECEIVED `%a` OVER TCP" Bytestring.pp msg);
      let msg = Bytestring.to_string msg in
      Logger.error (fun f ->
        Option.iter
          (fun pid -> f "TUI IS ACCESSIBLE %a" Pid.pp pid)
          (Process.where_is tui_name));
      send_by_name ~name:tui_name @@ Received msg
    | Ok msg ->
      Logger.error (fun f -> f "RECEIVED `%a` OVER TCP" Bytestring.pp msg);
      let raw = Bytestring.to_string msg |> String.trim in
      let is_ping = String.starts_with ~prefix:acknowleged raw in
      Logger.error (fun f -> f "RECEIVED IS PING? %b" is_ping);
      let msg_to_tui = if is_ping then Acknowleged else Received raw in
      send recv msg_to_tui
    | Error `Would_block | (exception Syscall_timeout) -> ()
    | Error err ->
      send_err err;
      shutdown ()
  ;;

  let start_loop state =
    let rec loop { role; reader; conn; recv; _ } =
      assert_tui ();
      match receive_any ~after:10L () with
      | exception Receive_timeout ->
        read recv role reader;
        loop state
      | exception e -> send_by_name ~name:tui_name @@ Err (Printexc.to_string e)
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
    register name pid
  ;;
end

module SocketWriter = struct
  (** Contains initializer and helpers for process which handles sending message over TCP stream *)

  (** State - holds the writer used to write to the TCP stream *)
  type t = { writer : TcpStream.t IO.Writer.t }

  (** Process name *)
  let name = "Chat.sender"

  (* Set namespace for logger *)
  open Logger.Make (struct
      let namespace = [ "chat"; "sender" ]
    end)

  (** [send t string] Attempts to send a message,
      recursing if doing so would block
      it forwards any other errors to the TUI *)
  let rec send ({ writer } as state) buf =
    let write = IO.write_all writer ~buf in
    match write with
    | Ok () ->
      Logger.error (fun f -> f "Sent!");
      let flushed = IO.flush writer in
      Result.iter_error send_err flushed
    | Error `Would_block ->
      yield ();
      send state buf
    | Error err ->
      for _ = 1 to 100 do
        Logger.error (fun f -> f "ERROR WRITING: %a" IO.pp_err err)
      done;
      send_err err;
      shutdown ()
  ;;

  (** [send_message Writer.t string] spawns a process that calls {send} 
      with the string passsed to it *)
  let send_message writer message =
    let buf = message ^ "\r\n" in
    let _ = spawn @@ fun () -> send writer buf in
    ()
  ;;

  (** [loop unit] process loop,
      awaits messages and acts on them *)
  let rec loop state =
    match receive_any () with
    | Send msg ->
      send_message state msg;
      loop state
    | Close -> shutdown ()
    | _ -> loop state
  ;;

  (** [start_link TcpStream.t] creates a Writer.t from the TcpStream.t 
       spawns a process that runs {loop}
       and registers the name "Chat.sender" to its process ID *)
  let start_link conn =
    let writer = TcpStream.to_writer conn in
    let pid = spawn_link @@ fun () -> loop { writer } in
    register name pid;
    pid
  ;;
end

module SocketReader = struct
  (** contains initializer and helpers for process which
      handles reading from TCP stream and forwarding messages to TUI *)

  (** State - holds the role (Host/Client), TCP stream, and a buffer *)
  type t =
    { conn : TcpStream.t
    ; role : role
    }

  (** Process name *)
  let name = "Chat.listener"

  (* Set namespace for logger *)
  open Logger.Make (struct
      let namespace = [ "chat"; "reader" ]
    end)

  (** [read ?string Iovec.t TcpStream.t] reads from TCP stream,
      recursing if the string received has a length of > 0 and
      doesn't end in a carriage return + newline.
      On recursion, it concatenates the string received in the
      previous iteration with the one currenly received *)
  let rec read ?(carry_over = "") conn =
    let* bs = Bytestring.with_bytes ~capacity:4096 @@ TcpStream.read conn in
    let raw = Bytestring.to_string bs in
    let is_eol = String.ends_with ~suffix:"\r\n" raw in
    let carry_over = carry_over ^ raw in
    if is_eol || String.length raw = 0
    then (
      yield ();
      read ~carry_over conn)
    else Ok carry_over
  ;;

  (** [listen t] recursively {read}s from TCP stream, 
      checking if the string received is the acknowledged flag 
      and sending messages to the TUI as necessary *)
  let rec listen ({ conn; role } as state) =
    match read conn with
    | Ok msg when role = Client ->
      Logger.error (fun f ->
        f "LISTENER RECEIVED: %s WHICH IS %d CHARS" msg (String.length msg));
      let is_acknowledged_flag =
        String.trim msg |> String.starts_with ~prefix:acknowleged
      in
      let msg_to_tui = if is_acknowledged_flag then Acknowleged else Received msg in
      send_by_name ~name:tui_name msg_to_tui;
      listen state
    | Ok msg ->
      Logger.error (fun f ->
        f "LISTENER RECEIVED: %s which has %d characters" msg (String.length msg));
      send_by_name ~name:tui_name @@ Received msg;
      listen state
    | Error `Would_block | Error `Timeout ->
      if Random.float 1. > 0.99999 then Logger.error (fun f -> f "WOULD_BLOCK/TIMEOUT");
      listen state
    | Error err ->
      send_err err;
      shutdown ()
  ;;

  (** [start_link role TcpStream.t] begins the listener process and binds
      the name "Chat.listener" to its process ID *)
  let start_link role conn =
    let pid = spawn_link @@ fun () -> listen { conn; role } in
    register name pid;
    send_by_name ~name:tui_name Connected;
    pid
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
  assert_tui ();
  send_by_name ~name:tui_name Connected;
  Ok (Host, conn)
;;

(** [init_client uri] connects socket to URI,
    Sends "Connected" message to TUI
    and returns state *)
let init_client uri =
  let* sockaddr = Addr.of_uri uri in
  let* conn = TcpStream.connect sockaddr in
  assert_tui ();
  send_by_name ~name:tui_name Connected;
  Ok (Client, conn)
;;

module SingleProcess = struct
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
           Handler.start recv role conn;
           loop ()
         | Error err ->
           send_by_name ~name:tui_name @@ Err (string_of_error err);
           loop ())
      (* User is starting in client mode *)
      | StartClient (uri, recv) ->
        Logger.error (fun f -> f "RECEIVED `Startserver` FROM TUI");
        (match init_client uri with
         | Ok (role, conn) ->
           Handler.start recv role conn;
           loop ()
         | Error err ->
           send_by_name ~name:tui_name @@ Err (string_of_error err);
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
    Logger.set_log_level (Some Logger.Debug);
    let pid = spawn_link handler in
    register chat_name pid;
    Option.iter link (Process.where_is tui_name);
    Ok pid
  ;;
end

module MultiProcess = struct
  let name = "Chat"

  (** [handler unit] Listens for initialization or close messages from TUI
      From there it initializes the TCP connection and child processes with
      the correct state *)
  let handler () =
    let rec loop () =
      match receive_any () with
      (* User is starting in host mode *)
      | StartServer (port, _) ->
        Logger.error (fun f -> f "RECEIVED `Startserver` FROM TUI");
        (match init_server port with
         | Ok (role, conn) ->
           let _sender = SocketWriter.start_link conn in
           let _listener = SocketReader.start_link role conn in
           loop ()
         | Error err ->
           send_by_name ~name:tui_name @@ Err (string_of_error err);
           loop ())
      (* User is starting in client mode *)
      | StartClient (uri, _) ->
        Logger.error (fun f -> f "RECEIVED `Startserver` FROM TUI");
        (match init_client uri with
         | Ok (role, conn) ->
           let _sender = SocketWriter.start_link conn in
           let _listener = SocketReader.start_link role conn in
           loop ()
         | Error err ->
           send_by_name ~name:tui_name @@ Err (string_of_error err);
           loop ())
      (* User has quit app *)
      | Close -> shutdown ()
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
