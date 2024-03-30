open Minttea
open Leaves
open Utils
module Logger = Riot.Logger

type connection =
  | Waiting
  | Open of Riot.Pid.t
  | Shutdown
  | Fatal of string

type model =
  | Menu of int
  | Chat of chat

and chat =
  { role : role
  ; connection : connection
  ; acknowleged : message_state
  ; text : Text_input.t
  ; spinner : Sprite.t option
  ; history : (role * string) list
  }

and message_state =
  | Sent of float
  | HostAcknowleged

let string_of_conn = function
  | Waiting -> "Waiting . . ."
  | Open _ -> "Open"
  | Shutdown -> "Shutdown"
  | Fatal reason -> Printf.sprintf "Fatal error: %s" reason
;;

let color (r, g, b) =
  let hex_str n = Printf.sprintf "%02X" n in
  Spices.color @@ Printf.sprintf "#%s%s%s" (hex_str r) (hex_str g) (hex_str b)
;;

let error_color = color (255, 1, 1)
let info_color = color (0, 98, 97)
let other_user_color = color (128, 128, 1)
let initial_model = Menu 0

let default_host_model =
  { role = Host
  ; connection = Waiting
  ; acknowleged = HostAcknowleged
  ; text = Text_input.empty ()
  ; spinner = Some Spinner.points
  ; history = [ Host, "Connecting . . ." ]
  }
;;

let default_client_model =
  { role = Client
  ; connection = Waiting
  ; acknowleged = HostAcknowleged
  ; text = Text_input.empty ()
  ; spinner = Some Spinner.points
  ; history = [ Client, "connecting . . ." ]
  }
;;

(** for viewing/updating the menu page:
    a four-tuple of label, role, initial state, and message to chat handler *)
let menu () =
  [ "Host a server", Host, default_host_model, StartServer (8080, Riot.Pid.zero)
  ; ( "Connect to a server"
    , Client
    , default_client_model
    , StartClient (Uri.of_string "//0.0.0.0:8080", Riot.Pid.zero) )
  ]
;;

(** [bound_idx f idx] ensures that f idx stays within the range of possible menu options *)
let bound_idx f idx = (f idx mod 2 |> ( + ) 2) mod 2

(** [time unit] gets the current time as seconds since the unix epoch *)
let time () = Unix.gettimeofday ()

(** [get_elapsed msg-state] takes time of sent messssage, subtracts it from current time and converts to a string *)
let get_elapsed acknowleged =
  match acknowleged with
  | Sent init_time -> Unix.gettimeofday () -. init_time |> string_of_float
  | _unreachable -> ""
;;

(** [update_menu choice event] handles events for menu "page"
    this includes selecting the role (Host | Client)*)
let update_menu idx = function
  | Event.KeyDown (Event.Up, _) -> Menu (bound_idx pred idx), Command.Noop
  | Event.KeyDown (Event.Down, _) -> Menu (bound_idx succ idx), Command.Noop
  | Event.KeyDown (Event.Enter, _) ->
    let menu = menu () in
    let _, role, model, msg = List.nth menu idx in
    let model =
      match role with
      | Host -> Chat model
      | Client -> Chat { model with acknowleged = Sent (Unix.gettimeofday ()) }
    in
    let msg =
      match msg with
      | StartServer (port, _) -> StartServer (port, Riot.self ())
      | StartClient (uri, _) -> StartClient (uri, Riot.self ())
      | _unreachable -> failwith "entered unreachable branch: tui.ml 105"
    in
    Riot.send_by_name ~name:Server.Handler.name msg;
    model, Command.Hide_cursor
  | Event.KeyDown (Event.Escape, _) -> Menu idx, Command.Quit
  | Event.Custom (Utils.Connected _) ->
    for _ = 1 to 100 do
      Logger.error (fun f -> f {|RECEIVED "Connected" IN MENU UPDATE|})
    done;
    Menu idx, Command.Noop
  | _ -> Menu idx, Command.Noop
;;

(** [update_chat model] handles event for chat "page" *)
let update_chat ({ role; acknowleged; text; history; spinner; connection } as model)
  = function
  | Event.Custom (Utils.Err e) ->
    Chat { model with history = (role, e) :: history }, Command.Noop
  | Event.Frame now ->
    let spinner = Option.map (Sprite.update ~now) spinner in
    Chat { model with spinner }, Command.Noop
  | Event.Custom (Received msg) ->
    Logger.error (fun f -> f "TUI RECEIVED MESSAGE: %s" msg);
    ( Chat
        { model with
          history = ((if role = Client then Host else Client), msg) :: history
        }
    , Command.Noop )
  | Event.Custom Acknowleged when role = Client ->
    let msg_body =
      Printf.sprintf "%s <> %ss" (Text_input.current_text text) (get_elapsed acknowleged)
    in
    let model =
      Chat
        { model with
          acknowleged = HostAcknowleged
        ; text = Text_input.empty ()
        ; history = (role, msg_body) :: history
        }
    in
    model, Command.Noop
  | Event.Custom (Connected handler) ->
    for _ = 1 to 20 do
      Logger.error (fun f -> f "TUI RECEIVED CONNECTED")
    done;
    let model =
      if role = Client
      then
        Chat
          { model with
            connection = Open handler
          ; acknowleged = HostAcknowleged
          ; history =
              (Host, Printf.sprintf "Connected to host in %ss" (get_elapsed acknowleged))
              :: history
          }
      else
        Chat
          { model with
            connection = Open handler
          ; history = (Client, "Established connection with client") :: history
          }
    in
    model, Command.Noop
  | Event.Custom Closed -> Chat { model with connection = Shutdown }, Command.Noop
  | Event.KeyDown (Event.Enter, _) when role = Client ->
    (* Don't send multiple messages if a message is still pending *)
    let model =
      match connection, acknowleged with
      | Open handler, HostAcknowleged ->
        let msg = Text_input.current_text text in
        Riot.send handler (Send msg);
        Chat { model with acknowleged = Sent (time ()) }
      | _ -> Chat model
    in
    model, Command.Noop
  | Event.KeyDown (Event.Enter, _) ->
    let msg = Text_input.current_text text in
    let name = Server.TcpServer.name in
    Riot.send_by_name ~name @@ Send msg;
    ( Chat { model with text = Text_input.empty (); history = (role, msg) :: history }
    , Command.Noop )
  | Event.KeyDown (Event.Escape, _) ->
    (match connection with
     | Open handler -> Riot.send handler Close
     | _ -> ());
    Menu 0, Command.Hide_cursor
  | key ->
    Logger.error (fun f -> f "Event: %a" Event.pp key);
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

let info_style =
  Spices.(default |> margin_top 2 |> margin_left 2 |> fg info_color |> build)
;;

let view_menu current_idx =
  let open Spices in
  let apply_list_style = default |> margin_top 2 |> margin_bottom 2 |> build in
  let place_cursor idx' choice =
    let cursor = if current_idx = idx' then ">" else "-" in
    Format.sprintf "%s %s" cursor choice
  in
  let options =
    List.mapi (fun i (choice, _, _, _) -> place_cursor i choice) (menu ())
    |> String.concat "\n"
  in
  apply_list_style "Hi! What would you like to do?\n%s\nPress `esc` to exit" options
;;

let view_chat { role; connection; text; spinner; history; _ } =
  let open Spices in
  let apply_role_style user (role, body) =
    let other_user_style = default |> fg other_user_color |> build in
    match user, role with
    | Host, Client -> other_user_style "Client" ^ ": " ^ body
    | Host, Host | Client, Client ->
      let apply_user_style = Spices.(default |> fg info_color |> build) in
      apply_user_style "You" ^ ": " ^ body
    | Client, Host -> other_user_style "Host" ^ ": " ^ body
  in
  match role, connection with
  | _, Fatal message ->
    let apply_error_style = default |> fg error_color |> build in
    apply_error_style "Fatal error: %s" message
  | Host, Waiting ->
    let sprite = Option.map Sprite.view spinner |> Option.value ~default:"" in
    info_style "Starting server and waiting for a connection %s" sprite
  | Client, Waiting ->
    let sprite = Option.map Sprite.view spinner |> Option.value ~default:"" in
    info_style "Connecting to server %s" sprite
  | _, Open _ ->
    let history =
      List.rev history |> List.map (apply_role_style role) |> String.concat "\n"
    in
    let history_syle = default |> margin_top 2 |> margin_left 2 |> build in
    let input_style = default |> margin_bottom 2 |> margin_left 2 |> build in
    let current_input = Text_input.view text in
    let text_input = input_style "\n%s" in
    history_syle "%s\n" history ^ text_input current_input
  | Host, Shutdown ->
    info_style "Client connection closed, wait for another if you'd like!%s" ""
  | Client, Shutdown -> info_style "Connection closed by host :/%s" ""
;;

let view = function
  | Menu idx -> view_menu idx
  | Chat state -> view_chat state
;;

let init _ = Command.Noop
let app = Minttea.app ~init ~update ~view
let run_tui () = Minttea.run ~initial_model @@ app ()

let start () =
  let open Riot in
  Logger.set_log_level None;
  let pid = spawn_link run_tui in
  register tui_name pid;
  Ok pid
;;
