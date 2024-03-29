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
let string_of_error (r : [> IO.io_error ]) =
  match r with
  | `No_info -> Printf.sprintf "No info\r\n%!"
  | `Connection_closed -> Printf.sprintf "Connection closed\r\n%!"
  | `Exn exn -> Printf.sprintf "Exn: %S\r\n%!" (Printexc.to_string exn)
  | `Unix_error err -> Printf.sprintf "Unix error: %S\r\n%!" (Unix.error_message err)
  | _ -> Printf.sprintf "other error"
;;

let send_err err_msg = send_by_name ~name:tui_name @@ Err err_msg

module SocketWriter = struct
  (** State - holds the writer used to write to the TCP stream *)
  type t = { writer : Socket.stream_socket IO.Writer.t }

  (** Process name *)
  let name = chat_name ^ ".sender"

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
      Result.iter_error (string_of_error >> send_err) flushed
    | Error `Would_block ->
      yield ();
      send state buf
    | Error err ->
      Logger.error (fun f -> f "ERROR WRITING: %a" IO.pp_err err);
      send_err (string_of_error err);
      shutdown ()
  ;;

  (** [send_message Writer.t string] spawns a process that calls {send} 
      with the string passsed to it *)
  let send_message writer message =
    let buf = message ^ "\r\n" in
    let _ = spawn_link @@ fun () -> send writer buf in
    ()
  ;;

  (** [loop unit] process loop,
      awaits messages and acts on them *)
  let rec loop state =
    assert_tui ();
    match receive () with
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
    assert_tui ();
    let writer = TcpStream.to_writer conn in
    let pid = spawn_link @@ fun () -> loop { writer } in
    register name pid;
    pid
  ;;
end

module SocketReader = struct
  (** State - holds the role (Host/Client), TCP stream, and a buffer *)
  type t =
    { conn : TcpStream.t
    ; role : role
    ; bufs : IO.Iovec.t
    }

  (** Process name *)
  let name = chat_name ^ ".listener"

  (* Set namespace for logger *)
  open Logger.Make (struct
      let namespace = [ "chat"; "reader" ]
    end)

  (** [read ?string Iovec.t TcpStream.t] reads from TCP stream,
      recursing if the string received has a length of > 0 and
      doesn't end in a carriage return + newline.
      On recursion, it concatenates the string received in the
      previous iteration with the one currenly received *)
  let rec read ?(carry_over = "") bufs conn =
    let* bytes_read = TcpStream.receive ~bufs conn in
    let raw = IO.Iovec.into_string bufs in
    Logger.error (fun f -> f "LISTENER RECEIVED: %s" raw);
    let has_carry_over = not (String.ends_with ~suffix:"\r\n" raw) in
    if has_carry_over || bytes_read = 0
    then (
      let carry_over = carry_over ^ raw in
      yield ();
      read ~carry_over bufs conn)
    else Ok carry_over
  ;;

  (** [listen t] recursively {read}s from TCP stream, 
      checking if the string received is the acknowledged flag 
      and sending messages to the TUI as necessary *)
  let rec listen ({ conn; role; bufs } as state) =
    assert_tui ();
    match read bufs conn with
    | Ok msg when role = Client ->
      Logger.error (fun f -> f "LISTENER RECEIVED: %s" msg);
      let is_acknowledged_flag =
        String.trim msg |> String.starts_with ~prefix:acknowleged
      in
      let msg_to_tui = if is_acknowledged_flag then Acknowleged else Received msg in
      send_by_name ~name:tui_name msg_to_tui;
      listen state
    | Ok msg ->
      Logger.error (fun f -> f "LISTENER RECEIVED: %s" msg);
      send_by_name ~name:tui_name @@ Received msg;
      listen state
    | Error `Would_block | Error `Timeout ->
      Logger.error (fun f -> f "WOULD BLOCK/TIMEOUT");
      listen state
    | Error err ->
      send_err (string_of_error err);
      shutdown ()
  ;;

  (** [start_link role TcpStream.t] begins the listener process and binds
      the name "Chat.listener" to its process ID *)
  let start_link role conn =
    assert_tui ();
    let bufs = IO.Iovec.create ~size:1024 () in
    let pid = spawn_link @@ fun () -> listen { conn; role; bufs } in
    register name pid;
    pid
  ;;
end

(** [init_server int state] creates a TCP socket and begins listening for connections on it
    Once a connection is established it sends a "Connected" message to the TUI
    and returns state *)
let init_server port =
  let* socket = TcpListener.bind ~port () in
  Logger.error (fun f -> f "Created server on port %d" port);
  let* conn = TcpListener.accept socket in
  Logger.error (fun f -> f "Connected with %a" Addr.pp (snd conn));
  send_by_name ~name:tui_name Connected;
  Ok (Host, fst conn)
;;

(** [init_client uri] connects socket to URI,
    Sends "Connected" message to TUI
    and returns state *)
let init_client uri =
  let* sockaddr = Addr.of_uri uri in
  let* conn = TcpStream.connect sockaddr in
  send_by_name ~name:tui_name Connected;
  Ok (Client, conn)
;;

(** [handler unit] Listens for initialization or close messages from TUI
    From there it initializes the TCP connection and child processes with
    the correct state *)
let handler () =
  let rec loop () =
    match receive () with
    (* User is starting in host mode *)
    | StartServer port ->
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
    | StartClient uri ->
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
  Logger.set_log_level (Some Logger.Error);
  let pid = spawn_link handler in
  register chat_name pid;
  Ok pid
;;
