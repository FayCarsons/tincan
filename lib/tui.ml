(* Terminal UI following the Elm MVC model *)

open Minttea
open Leaves
open Utils
module Logger = Riot.Logger

type model =
  | Menu of int  (** Menu page *)
  | Config of config
  | Chat of chat  (** Chat page *)

and config = {
  text : Text_input.t;
  role : role;
  init_message : (Riot.Message.t, string) result;
}

and chat = {
  role : role;
  connection : connection;
  acknowleged : message_state;
  text : Text_input.t;  (** Text input buffer *)
  spinner : Sprite.t option;  (** Loading sprite *)
  history : (role * string) list;  (** Chat message history *)
}

and connection =
  | Waiting
  | Open of Riot.Pid.t
      (** The connection is open so we store the process' ID to send commands *)
  | Shutdown
      (** Connection has been closed, the process has been shut down and socket closed *)

and message_state =
  | Sent of float  (** Pending acknowledgement ping, store time sent *)
  | HostAcknowleged

(** gets the current time as seconds since the unix epoch *)
let time () = Unix.gettimeofday ()

(** [get_elapsed msg-state] takes time of sent messssage, subtracts it from current time,
    getting the total elapsed time, and converts to a string *)
let get_elapsed acknowleged =
  match acknowleged with
  | Sent init_time ->
      let time = Unix.gettimeofday () -. init_time in
      String.sub (string_of_float time) 0 7
  | _unreachable -> ""

(* For message formatting *)
let not_role = function Host -> Client | Client -> Host
let string_of_role = function Host -> "Host" | Client -> "Client"
let initial_model = Menu 0

(* Update fns *)

(* Menu *)

(** for viewing/updating the menu page and initializing chat state:
    a four-tuple of label, role, initial state, and init message.
    we initialize the mesages that will be sent to the server with fake process IDs
    and insert the actual TUI process ID right before sending *)
let menu =
  [
    ("Host a server", Host, StartServer (8080, Riot.Pid.zero));
    ( "Connect to a server",
      Client,
      StartClient (Uri.of_string "//0.0.0.0:8080", Riot.Pid.zero) );
  ]

(** [bound_idx f idx] ensures that f idx stays within the range of possible menu options *)
let bound_idx f idx =
  let len = List.length menu in
  (f idx mod len |> ( + ) len) mod 2

(** [update_menu menu_index event] handles events for menu page
    returning an initial config when a chocie is made *)
let update_menu idx = function
  | Event.KeyDown (Event.Up, _) -> (Menu (bound_idx pred idx), Command.Noop)
  | Event.KeyDown (Event.Down, _) -> (Menu (bound_idx succ idx), Command.Noop)
  | Event.KeyDown (Event.Enter, _) ->
      let _, role, init = List.nth menu idx in
      ( Config { text = Text_input.empty (); role; init_message = Ok init },
        Command.Show_cursor )
  | Event.KeyDown (Event.Escape, _) -> (Menu idx, Command.Quit)
  | _ -> (Menu idx, Command.Noop)

(* Config page *)

let default_port = 8080
let default_uri = "127.0.0.1:8080"

(* For initialization *)
let default_host_model =
  {
    role = Host;
    connection = Waiting;
    acknowleged = HostAcknowleged;
    text = Text_input.empty ();
    spinner = Some Spinner.meter;
    history = [];
  }

let default_client_model =
  {
    role = Client;
    connection = Waiting;
    acknowleged = HostAcknowleged;
    text = Text_input.empty ();
    spinner = Some Spinner.mini_dot;
    history = [];
  }

(** [validate_uri uri] asserts that uri has a host *)
let validate_uri uri =
  let as_t = Uri.of_string ("//" ^ uri) in
  if Uri.host as_t |> Option.is_some then Ok as_t
  else Error "Uri must contain a host!"

(** [validate_port port_string] asserts that port is a number and is currently open *)
let validate_port port =
  let in_range port =
    if 0 < port && port <= 65535 then Ok port else Error "Invalid port number"
  in
  int_of_string_opt port
  |> Option.to_result ~none:"Port is not a number"
  |> Fun.flip Result.bind in_range

(* [validate_config model] validates the port/address input by the user,
   initializing the chat page if valid or returning an error state if not *)
let validate_config ({ text; role; _ } as model) =
  let current_text = Text_input.current_text text in
  let validate_host port_str =
    match validate_port port_str with
    | Ok port ->
        Riot.send_by_name ~name:Server.Handler.name
        @@ StartServer (port, Riot.self ());
        Chat default_host_model
    | Error err -> Config { model with init_message = Error err }
  in
  let validate_client uri =
    match validate_uri uri with
    | Ok uri ->
        Riot.send_by_name ~name:Server.Handler.name
        @@ StartClient (uri, Riot.self ());
        Chat { default_client_model with acknowleged = Sent (time ()) }
    | Error err -> Config { model with init_message = Error err }
  in
  if role = Host then
    let maybe_port =
      if String.length current_text = 0 then string_of_int default_port
      else current_text
    in
    validate_host maybe_port
  else
    let uri =
      if String.length current_text = 0 then default_uri else current_text
    in
    validate_client uri

(** [update_config model event] handles user input for the config page *)
let update_config ({ text; _ } as model) = function
  | Event.KeyDown (Event.Escape, _) -> (Menu 0, Command.Hide_cursor)
  | Event.KeyDown (Event.Backspace, _) ->
      let current_text = Text_input.current_text text in
      let butlast =
        String.sub current_text 0 (String.length current_text - 2)
      in
      ( Config { model with text = Text_input.set_text butlast text },
        Command.Noop )
  | Event.KeyDown (Event.Enter, _) ->
      (validate_config model, Command.Hide_cursor)
  | key ->
      (Config { model with text = Text_input.update text key }, Command.Noop)

(* Chat *)

(** [message_acknowledged model] updates state when ping is received from host *)
let message_acknowledged ({ role; text; acknowleged; history; _ } as model) =
  let msg = Text_input.current_text text in
  let elapsed = get_elapsed acknowleged in
  let formatted = Printf.sprintf "%s : received in %ss" msg elapsed in
  Chat
    {
      model with
      acknowleged = HostAcknowleged;
      text = Text_input.empty ();
      history = (role, formatted) :: history;
    }

(** [connected model] updates state when `Connected` message is received from TCP handler *)
let connected ({ role; history; acknowleged; _ } as model) handler =
  if role = Client then
    Chat
      {
        model with
        connection = Open handler;
        acknowleged = HostAcknowleged;
        spinner = None;
        history =
          ( role,
            Printf.sprintf "Connected to host in %ss" (get_elapsed acknowleged)
          )
          :: history;
      }
  else
    Chat
      {
        model with
        connection = Open handler;
        spinner = None;
        history = (role, "Established connection with client") :: history;
      }

(** [send_message model] attempts to send a message when in client mode.
    this acts as a no-op if a message is already pending
    oherwise we send the message to the TCP handler and append it to our chat history *)
let send_message ({ role; connection; acknowleged; history; text; _ } as model)
    =
  match (connection, acknowleged) with
  | Open handler, HostAcknowleged ->
      let msg = Text_input.current_text text in
      Riot.send handler (Send msg);
      let model =
        if role = Client then { model with acknowleged = Sent (time ()) }
        else { model with history = (role, msg) :: history }
      in
      Chat model
  | _ ->
      Chat
        {
          model with
          history =
            (role, "Cannot send messages while pending a response") :: history;
        }

(** [update_chat model event] handles event for chat page *)
let update_chat
    ({ role; acknowleged; text; history; spinner; connection } as model) =
  function
  | Event.Frame now when connection = Waiting ->
      (* Update sprites if we're waiting for a connection *)
      let spinner = Option.map (Sprite.update ~now) spinner in
      (Chat { model with spinner }, Command.Noop)
  (* Custom events are messages sent from TCP connection process *)
  | Event.Custom (Utils.Err e) ->
      (* print errors to chat history *)
      (Chat { model with history = (role, e) :: history }, Command.Noop)
  | Event.Custom (Received msg) ->
      let msg = String.trim msg in
      ( Chat { model with history = (not_role role, msg) :: history },
        Command.Noop )
  | Event.Custom Acknowleged when role = Client ->
      (message_acknowledged model, Command.Noop)
  | Event.Custom (Connected handler) -> (connected model handler, Command.Noop)
  | Event.Custom Closed ->
      (* When a connection is closed,
               either return to a waiting state in the case of the host
               else return the current state with connection = Shutdown *)
      let model =
        if role = Host then Chat default_host_model
        else Chat { model with connection = Shutdown }
      in
      (model, Command.Noop)
  | Event.KeyDown (Event.Enter, _) -> (send_message model, Command.Noop)
  | Event.KeyDown (Event.Escape, _) ->
      (* Escape closes the server process and TCP connection,
         and takes us back to the menu *)
      (match connection with
      | Open handler -> Riot.send handler Close
      | _ -> ());
      (Menu 0, Command.Hide_cursor)
  | key ->
      (* Other keys are concatenated onto our text input buffer,
         if our last message isn't pending *)
      let model =
        if acknowleged = HostAcknowleged then
          Chat { model with text = Text_input.update text key }
        else Chat model
      in
      (model, Command.Noop)

(** [update Event.t model] main update fn
    matches on model and dispatches args to correct update fn*)
let update event model =
  match model with
  | Menu idx -> update_menu idx event
  | Config model -> update_config model event
  | Chat model -> update_chat model event

(* Render fns & utilities *)

(* Colors *)
let error_color = Spices.color "#FF0101"
let main_color = Spices.color "#4756A1"
let other_user_color = Spices.color "#F7C0C0"
let info_color = Spices.color "#EA5D89"

(* Styling *)
let user_style = Spices.(default |> fg main_color |> build)
let other_user_style fmt = Spices.(default |> fg other_user_color |> build) fmt

let info_style fmt =
  Spices.(
    default |> italic true |> padding_top 2 |> padding_left 2 |> fg info_color
    |> build)
    fmt

let error_style =
  Spices.(
    default |> italic true |> padding_top 2 |> padding_left 2 |> fg error_color
    |> build)

(** [view_menu int] renders the menu *)
let view_menu current_idx =
  let menu_style =
    Spices.(default |> padding_top 2 |> padding_bottom 2 |> build)
  in
  let place_cursor idx' choice =
    let cursor = if current_idx = idx' then "\240\159\152\184" else "-" in
    Format.sprintf "%s %s" cursor choice
  in
  let options =
    List.mapi (fun i (choice, _, _) -> place_cursor i choice) menu
    |> String.concat "\n"
  in
  menu_style "Hi! Welcome to TinCan, what would you like to do?\n%s\n%s" options
  @@ Spices.(default |> italic true |> build) "Press `esc` to close"

let view_config { text; init_message; role } =
  let current_text = Text_input.current_text text in
  match role with
  | Host ->
      let current_port =
        if String.length current_text = 0 then string_of_int default_port
        else current_text
      in
      let err =
        match init_message with Error e -> e | _ -> "" |> error_style
      in
      info_style "Please enter the port you'd like to use: "
      ^ current_port ^ "\n" ^ err
  | Client ->
      let current_uri =
        if String.length current_text = 0 then default_uri else current_text
      in
      let err =
        match init_message with Error e -> e | _ -> "" |> error_style
      in
      info_style
        "Please enter the inet address or host you'd like to connect to: "
      ^ current_uri ^ "\n" ^ err

let history_syle = Spices.(default |> border Border.rounded |> build)
let input_style = Spices.(default |> margin_bottom 2 |> margin_left 2 |> build)

(** [view_chat chat] renders the chat page *)
let view_chat { role; connection; text; spinner; history; _ } =
  (* Apply styling based on our current role and the role attached to the message *)
  let apply_role_style user (role, msg) =
    match (user, role) with
    | Host, Client -> other_user_style "Client" ^ ": " ^ msg
    | Client, Host -> other_user_style "Host" ^ ": " ^ msg
    | _, _ -> user_style "You" ^ ": " ^ msg
  in
  (* We match on the current role and connection state *)
  match (role, connection) with
  | Host, Waiting ->
      let sprite_view =
        Option.map Sprite.view spinner |> Option.value ~default:""
      in
      info_style "Waiting for a connection %s" sprite_view
  | Client, Waiting ->
      let sprite_view =
        Option.map Sprite.view spinner |> Option.value ~default:""
      in
      info_style "Connecting to host %s" sprite_view
  | _, Open _ ->
      let history =
        List.rev history
        |> List.map (apply_role_style role)
        |> String.concat "\n"
      in
      let current_input = Text_input.view text in
      let text_input = input_style "%s" current_input in
      history_syle "%s" history ^ "\n" ^ text_input
  | _, Shutdown -> info_style "Connection closed~%s" String.empty

(** [view model] calls the proper rendering function on the model based on its state *)
let view = function
  | Menu idx -> view_menu idx
  | Config model -> view_config model
  | Chat model -> view_chat model

(* Initialization *)

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
  Ok pid
