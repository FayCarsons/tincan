(* TCP server/client + handler process *)

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
  | `Unix_error err ->
      Printf.sprintf "Unix error: %S\r\n%!" (Unix.error_message err)
  | `Noop -> "NoOp\r\n"
  | `Eof -> "End of file\r\n"
  | `Closed -> "Closed\r\n"
  | `Process_down -> "Process down!\r\n"
  | _ -> Printf.sprintf "other error\r\n"

(** [send_err IO.io_error] sends an error to the TUI *)
let send_err recv err_msg = send recv @@ Err (string_of_error err_msg)

module Connection = struct
  (** Handles TCP connections and communicating with TUI and parent process *)

  open Logger.Make (struct
    let namespace = [ "tincan"; "connection" ]
  end)

  (** Process name *)
  let name = "Handler.connection"

  type t = {
    reader : TcpStream.t IO.Reader.t;
    writer : TcpStream.t IO.Writer.t;
    conn : TcpStream.t;
    role : role;
    recv : Pid.t;
  }
  (** State passed around process + helper fns *)

  (** [send_message state message] sends a string via the current TCP connection *)
  let rec send_message ({ writer; recv; _ } as state) message =
    let bufs = IO.Iovec.from_string @@ message ^ "\r\n" in
    Logger.error (fun f -> f "Sending messsage: %s" message);
    match IO.write_all_vectored writer ~bufs with
    | Ok _ -> (
        match IO.flush writer with
        | Ok () -> Logger.error (fun f -> f "Flushed writer!")
        | exception e -> send recv @@ Err (Printexc.to_string e)
        | Error err -> send_err recv err)
    | exception e ->
        let reason = Printexc.to_string e in
        Logger.error (fun f -> f "EXCEPTION: %s" reason);
        send recv @@ Err reason
    | Error `Would_block | Error `Timeout ->
        yield ();
        send_message state message
    | Error err ->
        Logger.error (fun f -> f "ERROR SENDING: %a" IO.pp_err err);
        send_err recv err

  (** [read state] attempts to read from TCP stream, ignoring timeouts and calls that would block 
      so that we can return to reading fom the mailbox/sending messages if the stream is empty *)
  let read ({ recv; role; reader; _ } as state) =
    match
      Bytestring.with_bytes ~capacity:4096 (fun buf ->
          IO.read ~timeout:10L reader buf)
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

  (** [start_loop state] starts the main process loop which alternates between attempting to 
      read the process mailbox, read the TCP stream, and write to the TCP stream *)
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

  (** [start receiver_process user_role tcp_connection] 
      creates initial state and starts the process  *)
  let start recv role conn =
    let reader = TcpStream.to_reader conn in
    let writer = TcpStream.to_writer conn in
    let pid =
      spawn_link @@ fun () -> start_loop { recv; reader; writer; conn; role }
    in
    register name pid;
    send recv (Connected pid)
end

(* Host initialization, broken down into smaller functions for testing *)

(** [get_socket port] creates a Socket.listen_socket on provided port *)
let get_socket port = TcpListener.bind ~port ()

(** [accept listen_socket] waits for a connection on port and returns a stream socket *)
let accept socket = TcpListener.accept socket |> Result.map fst

(** [init_server int state] creates a TCP socket and begins listening for connections on it
    Once a connection is established it sends a "Connected" message to the TUI
    and returns state *)
let init_server port =
  let* socket = get_socket port in
  let* conn = accept socket in
  Ok (Host, conn)

(** [init_client uri] connects socket to URI,
    Sends "Connected" message to TUI
    and returns state *)
let init_client uri =
  let* sockaddr = Addr.of_uri uri in
  let* conn = TcpStream.connect sockaddr in
  Ok (Client, conn)

module Handler = struct
  (** Supervisor, handles initializing and closing connections *)

  open Logger.Make (struct
    let namespace = [ "tincan"; "handler" ]
  end)

  let name = "Handler"

  (** [handler unit] Listens for initialization or close messages from TUI
      From there it initializes the TCP connection and child processes with
      the correct state *)
  let handler () =
    let rec loop () =
      match receive_any () with
      (* User is starting in host mode *)
      | StartServer (port, recv) -> (
          match init_server port with
          | Ok (role, conn) ->
              Connection.start recv role conn;
              loop ()
          | Error err ->
              send recv @@ Err (string_of_error err);
              loop ())
      (* User is starting in client mode *)
      | StartClient (uri, recv) -> (
          match init_client uri with
          | Ok (role, conn) ->
              Connection.start recv role conn;
              loop ()
          | Error err ->
              send recv @@ Err (string_of_error err);
              loop ())
      (* User has quit app *)
      | Close -> shutdown ()
      (* Unkonwn/irrelevant message*)
      | _ -> loop ()
    in
    loop ()

  (** [start unit] begins handler and registers name to its process ID *)
  let start () =
    let pid = spawn_link handler in
    register name pid;
    Ok pid
end
