open Riot
module Socket = Net.Socket
module Addr = Net.Addr
module TcpListener = Net.Tcp_listener
module TcpStream = Net.Tcp_stream

let ( let* ) = Result.bind

open Utils

type state =
  { role : role
  ; conn : Socket.stream_socket
  }

let string_of_error (r : [> IO.io_error ]) =
  match r with
  | `No_info -> Printf.sprintf "No info\r\n%!"
  | `Connection_closed -> Printf.sprintf "Connection closed\r\n%!"
  | `Exn exn -> Printf.sprintf "Exn: %S\r\n%!" (Printexc.to_string exn)
  | `Unix_error err -> Printf.sprintf "Unix error: %S\r\n%!" (Unix.error_message err)
  | _ -> Printf.sprintf "other error"
;;

let send_err err_msg = send_by_name ~name:tui_name @@ Err err_msg

let string_of_message = function
  | Send _ -> "send"
  | Acknowleged -> "acknowleged"
  | Received _ -> "received"
  | Close -> "close"
  | _ -> "unhandled"
;;

module SocketWriter = struct
  type t = { writer : Socket.stream_socket IO.Writer.t }

  let name = chat_name ^ ".sender"

  let rec send ({ writer } as state) buf =
    let write = IO.write_all writer ~buf in
    match write with
    | Ok () ->
      Logger.error (fun f -> f "Sent!");
      let flushed = IO.flush writer in
      Result.iter_error (string_of_error >> send_err) flushed
    | Error `Would_block | Error `Timeout ->
      yield ();
      send state buf
    | Error err ->
      Logger.error (fun f -> f "ERROR WRITING: %a" IO.pp_err err);
      send_err (string_of_error err);
      shutdown ()
  ;;

  let send_message writer message =
    let buf = message ^ "\r\n" in
    let _ = spawn_link @@ fun () -> send writer buf in
    ()
  ;;

  let rec loop state =
    match receive () with
    | Send msg ->
      send_message state msg;
      loop state
    | Received _ as msg ->
      send_by_name ~name:tui_name msg;
      loop state
    | Close -> shutdown ()
    | _ -> loop state
  ;;

  let start_link conn =
    assert (Process.where_is tui_name |> Option.is_some);
    let writer = TcpStream.to_writer conn in
    let pid = spawn_link @@ fun () -> loop { writer } in
    register name pid;
    pid
  ;;
end

module SocketReader = struct
  type t =
    { conn : TcpStream.t
    ; role : role
    ; bufs : IO.Iovec.t
    }

  let name = chat_name ^ ".listener"

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

  let rec listen ({ conn; role; bufs } as state) =
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

  let start_link role conn =
    assert (Process.where_is tui_name |> Option.is_some);
    let bufs = IO.Iovec.create ~size:1024 () in
    let pid = spawn_link @@ fun () -> listen { conn; role; bufs } in
    register name pid;
    pid
  ;;
end

(** [init_server int state] creates a TCP socket and begins listening for connections on it
    Accepts a `state` option so that it cannot be initialized twice *)
let init_server port =
  let* socket = TcpListener.bind ~port () in
  Logger.error (fun f -> f "Created server on port %d" port);
  let* conn = TcpListener.accept socket in
  Logger.error (fun f -> f "Connected with %a" Addr.pp (snd conn));
  send_by_name ~name:tui_name Connected;
  Ok { role = Host; conn = fst conn }
;;

(** [init_client uri] connects socket to URI *)
let init_client uri =
  let* sockaddr = Addr.of_uri uri in
  let* conn = TcpStream.connect sockaddr in
  send_by_name ~name:tui_name Connected;
  Ok { role = Client; conn }
;;

let handler () =
  let rec loop () =
    match receive () with
    | StartServer port ->
      (match init_server port with
       | Ok { role; conn } ->
         let _sender = SocketWriter.start_link conn in
         let _listener = SocketReader.start_link role conn in
         loop ()
       | Error err ->
         send_by_name ~name:tui_name @@ Err (string_of_error err);
         loop ())
    | StartClient uri ->
      (match init_client uri with
       | Ok { role; conn } ->
         let _sender = SocketWriter.start_link conn in
         let _listener = SocketReader.start_link role conn in
         send_by_name ~name:tui_name Connected;
         loop ()
       | Error err ->
         send_by_name ~name:tui_name @@ Err (string_of_error err);
         loop ())
    | Close -> shutdown ()
    | _ -> loop ()
  in
  loop ()
;;

let start () =
  Logger.set_log_level (Some Logger.Error);
  let pid = spawn_link handler in
  register chat_name pid;
  Ok pid
;;
