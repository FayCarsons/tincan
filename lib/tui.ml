(* Terminal UI following the Elm MVC model *)

open Minttea
open Leaves
open Utils
module Logger = Riot.Logger

type model =
  | Menu of int (** Menu page *)
  | Chat of chat (** Chat page *)

and chat =
  { role : role
  ; connection : connection
  ; acknowleged : message_state
  ; text : Text_input.t (** Text input buffer *)
  ; spinner : Sprite.t option (** Loading sprite *)
  ; history : (role * string) list (** Chat message history *)
  }

and connection =
  | Waiting
  | Open of Riot.Pid.t
      (** The connection is open so we store the process' ID to send commands *)
  | Shutdown
      (** Connection has been closed, the process has been shut down and socket closed *)

and message_state =
  | Sent of float (** Pending acknowledgement ping, store time sent *)
  | HostAcknowleged

let string_of_conn = function
  | Waiting -> "Waiting . . ."
  | Open _ -> "Open"
  | Shutdown -> "Shutdown"
;;

(** [color r g b] converts a int8 RGB three-tuple into a Spices color *)
let color (r, g, b) =
  let hex_str n = Printf.sprintf "%02X" n in
  Spices.color @@ Printf.sprintf "#%s%s%s" (hex_str r) (hex_str g) (hex_str b)
;;

let error_color = color (255, 1, 1)
let main_color = color (0, 255, 30)
let other_user_color = color (255, 255, 1)
let initial_model = Menu 0

(* Default models for initialization *)
let default_host_model =
  { role = Host
  ; connection = Waiting
  ; acknowleged = HostAcknowleged
  ; text = Text_input.empty ()
  ; spinner = Some Spinner.meter
  ; history = []
  }
;;

let default_client_model =
  { role = Client
  ; connection = Waiting
  ; acknowleged = HostAcknowleged
  ; text = Text_input.empty ()
  ; spinner = Some Spinner.mini_dot
  ; history = []
  }
;;

(** for viewing/updating the menu page and initializing chat state:
    a four-tuple of label, role, initial state, and init message.
    we initialize the mesages that will be sent to the server with fake process IDs
    and insert the actual TUI process ID right before sending *)
let menu =
  [ "Host a server", Host, default_host_model, StartServer (8080, Riot.Pid.zero)
  ; ( "Connect to a server"
    , Client
    , default_client_model
    , StartClient (Uri.of_string "//0.0.0.0:8080", Riot.Pid.zero) )
  ]
;;

(** [bound_idx f idx] ensures that f idx stays within the range of possible menu options *)
let bound_idx f idx =
  let len = List.length menu in
  (f idx mod len |> ( + ) len) mod 2
;;

(* Time fns *)
(* TODO: use Ptime instead *)

(** gets the current time as seconds since the unix epoch *)
let time () = Unix.gettimeofday ()

(** [get_elapsed msg-state] takes time of sent messssage, subtracts it from current time,
    getting the total elapsed time, and converts to a string *)
let get_elapsed acknowleged =
  match acknowleged with
  | Sent init_time ->
    let time = Unix.gettimeofday () -. init_time in
    String.sub (string_of_float time) 0 5
  | _unreachable -> ""
;;

(** [update_menu choice event] handles events for menu "page"
    this includes selecting the role (Host | Client) *)
let update_menu idx = function
  | Event.KeyDown (Event.Up, _) -> Menu (bound_idx pred idx), Command.Noop
  | Event.KeyDown (Event.Down, _) -> Menu (bound_idx succ idx), Command.Noop
  | Event.KeyDown (Event.Enter, _) ->
    (* When a choice is made we get our initial state for the chat model
       and the init message sent to the server,*)
    let _, role, model, msg = List.nth menu idx in
    let model =
      match role with
      | Client -> Chat { model with acknowleged = Sent (Unix.gettimeofday ()) }
      | _ -> Chat model
    in
    let msg =
      match msg with
      | StartServer (port, _) -> StartServer (port, Riot.self ())
      | StartClient (uri, _) -> StartClient (uri, Riot.self ())
      | _unreachable -> failwith "entered unreachable branch"
    in
    (* Send init message to server *)
    Riot.send_by_name ~name:Server.Handler.name msg;
    model, Command.Hide_cursor
  | Event.KeyDown (Event.Escape, _) -> Menu idx, Command.Quit
  | _ -> Menu idx, Command.Noop
;;

(** [update_chat model] handles event for chat page *)
let update_chat ({ role; acknowleged; text; history; spinner; connection } as model)
  = function
  (* Update sprites if we're waiting for a connection *)
  | Event.Frame now when connection = Waiting ->
    let spinner = Option.map (Sprite.update ~now) spinner in
    Chat { model with spinner }, Command.Noop
  (* CUSTOM EVENTS *)
  (* End app on error *)
  | Event.Custom (Utils.Err e) ->
    Chat { model with history = (role, e) :: history }, Command.Noop
  (* Msg received from TCP server *)
  | Event.Custom (Received msg) ->
    Chat { model with history = (not_role role, msg) :: history }, Command.Noop
  (* Received ping/acknowledged flag from host*)
  | Event.Custom Acknowleged when role = Client ->
    let msg = Text_input.current_text text in
    let elapsed = get_elapsed acknowleged in
    let formatted = Printf.sprintf "%s : received in %ss" msg elapsed in
    let model =
      Chat
        { model with
          acknowleged = HostAcknowleged
        ; text = Text_input.empty ()
        ; history = (role, formatted) :: history
        }
    in
    model, Command.Noop
  (*  Connection established *)
  | Event.Custom (Connected handler) ->
    let model =
      if role = Client
      then
        Chat
          { model with
            connection = Open handler
          ; acknowleged = HostAcknowleged
          ; history =
              (Client, Printf.sprintf "Connected to host in %ss" (get_elapsed acknowleged))
              :: history
          }
      else
        Chat
          { model with
            connection = Open handler
          ; history = (Host, "Established connection with client") :: history
          }
    in
    model, Command.Noop
  (* Connection closed *)
  | Event.Custom Closed -> Chat { model with connection = Shutdown }, Command.Noop
  (* Keyboard input *)
  (* Enter - send messsage if we aren't already waiting for one to be acknowledged*)
  | Event.KeyDown (Event.Enter, _) when role = Client ->
    let model =
      match connection, acknowleged with
      | Open handler, HostAcknowleged ->
        let msg = Text_input.current_text text in
        Riot.send handler (Send msg);
        Chat { model with acknowleged = Sent (time ()) }
      | _ ->
        Chat
          { model with
            history = (role, "Cannot send messages while pending a response") :: history
          }
    in
    model, Command.Noop
    (* If we aren't in client mode w can just send the message and cons it onto history *)
  | Event.KeyDown (Event.Enter, _) ->
    let msg = Text_input.current_text text in
    let name = Server.TcpServer.name in
    Riot.send_by_name ~name @@ Send msg;
    ( Chat { model with text = Text_input.empty (); history = (role, msg) :: history }
    , Command.Noop )
    (* Escape closes the server process and takes us back to the menu *)
  | Event.KeyDown (Event.Escape, _) ->
    (match connection with
     | Open handler -> Riot.send handler Close
     | _ -> ());
    Menu 0, Command.Hide_cursor
    (* Other keys are simply concatenated onto our text input buffer *)
  | key ->
    let model =
      if acknowleged = HostAcknowleged
      then Chat { model with text = Text_input.update text key }
      else Chat model
    in
    model, Command.Noop
;;

(** [update Event.t model] main update fn
    matches on model and dispatches args to correct update fn*)
let update event model =
  match model with
  | Menu idx -> update_menu idx event
  | Chat state -> update_chat state event
;;

let main_style =
  Spices.(default |> margin_top 2 |> margin_left 2 |> fg main_color |> build)
;;

(** [view_menu int] renders the menu *)
let view_menu current_idx =
  let open Spices in
  let apply_list_style = default |> margin_top 2 |> margin_bottom 2 |> build in
  let place_cursor idx' choice =
    let cursor = if current_idx = idx' then "\240\159\152\184" else "-" in
    Format.sprintf "%s %s" cursor choice
  in
  let options =
    List.mapi (fun i (choice, _, _, _) -> place_cursor i choice) menu
    |> String.concat "\n"
  in
  apply_list_style "Hi! What would you like to do?\n%s\nPress `esc` to exit" options
;;

(** [view_chat chat] renders the chat page *)
let view_chat { role; connection; text; spinner; history; _ } =
  let open Spices in
  (* Apply styling based on our current role and the role attached to the message *)
  let apply_role_style user (role, msg) =
    let other_user_style = default |> fg other_user_color |> build in
    match user, role with
    | Host, Client -> other_user_style "Client" ^ ": " ^ msg
    | Client, Host -> other_user_style "Host" ^ ": " ^ msg
    | _, _ ->
      let apply_user_style = Spices.(default |> fg main_color |> build) in
      apply_user_style "You" ^ ": " ^ msg
  in
  (* We match on the current role and connection state *)
  match role, connection with
  | Host, Waiting ->
    let sprite = Option.map Sprite.view spinner |> Option.value ~default:"" in
    main_style "Starting server and waiting for a connection %s" sprite
  | Client, Waiting ->
    let sprite = Option.map Sprite.view spinner |> Option.value ~default:"" in
    main_style "Connecting to server %s" sprite
  | _, Open _ ->
    let history =
      List.rev history |> List.map (apply_role_style role) |> String.concat "\n"
    in
    let history_syle = default |> margin_top 2 |> margin_left 2 |> build in
    let input_style = default |> margin_bottom 2 |> margin_left 2 |> build in
    let current_input = Text_input.view text in
    let text_input = input_style "%s" in
    history_syle "%s\n" history ^ text_input current_input
  | Host, Shutdown -> main_style "Client connection closed %s" String.empty
  | Client, Shutdown -> main_style "Connection closed by host :/%s" String.empty
;;

(** [view model] calls the proper rendering function on the model based on its state *)
let view = function
  | Menu idx -> view_menu idx
  | Chat state -> view_chat state
;;

(** [init _] gives us our first command, in this case we do nothing *)
let init _ = Command.Noop

(** The minttea app *)
let app = Minttea.app ~init ~update ~view

(** [run_tui unit] runs the app *)
let run_tui () = Minttea.run ~fps:30 ~initial_model @@ app ()

(** [start unit] initializes the Minttea app as a Riot process and returns the process id to the runtime *)
let start () =
  let open Riot in
  Logger.set_log_level None;
  let pid = spawn_link run_tui in
  register tui_name pid;
  Ok pid
;;
