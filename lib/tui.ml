open Minttea
open Leaves
open Utils
module Logger = Riot.Logger
module Sender = Server.SocketWriter

type connection =
  | Waiting
  | Open
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
  | Open -> "Open"
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
  ; history = []
  }
;;

let default_client_model =
  { role = Client
  ; connection = Waiting
  ; acknowleged = HostAcknowleged
  ; text = Text_input.empty ()
  ; spinner = Some Spinner.points
  ; history = []
  }
;;

let menu =
  [ "Host a server", Host, default_host_model, StartServer 8080
  ; ( "Connect to a server"
    , Client
    , default_client_model
    , StartClient (Uri.of_string "//127.0.0.1:8080") )
  ]
;;

(** [bound_idx f idx] ensures that f idx stays within the range of possible menu options *)
let bound_idx f idx = (f idx mod 2 |> ( + ) 2) mod 2

(** [update_menu choice event] Handles menu choice, returns initialized chat state
    when `Enter` is hit and a choice is made *)
let update_menu idx = function
  | Event.KeyDown Event.Up -> Menu (bound_idx pred idx), Command.Noop
  | Event.KeyDown Event.Down -> Menu (bound_idx succ idx), Command.Noop
  | Event.KeyDown Event.Enter ->
    let _, role, model, msg = List.nth menu idx in
    let model =
      match role with
      | Host -> Chat model
      | Client -> Chat { model with acknowleged = Sent (Unix.gettimeofday ()) }
    in
    Riot.send_by_name ~name:chat_name msg;
    model, Command.Hide_cursor
  | Event.KeyDown Event.Escape -> Menu idx, Command.Quit
  | _ -> Menu idx, Command.Noop
;;

(** [get_elapsed msg-state] takes time of sent messssage, subtracts it from current time and converts to a string *)
let get_elapsed acknowleged =
  match acknowleged with
  | Sent init_time -> Unix.gettimeofday () -. init_time |> string_of_float
  | _unreachable -> ""
;;

let update_chat ({ role; connection; acknowleged; text; spinner; history } as model)
  = function
  | Event.Custom (Utils.Err e) -> Chat { model with connection = Fatal e }, Command.Noop
  | Event.Frame now ->
    let spinner = Option.map (Sprite.update ~now) spinner in
    Chat { model with spinner }, Command.Noop
  | Event.Custom (Received msg) ->
    ( Chat
        { model with
          history = ((if role = Client then Host else Client), msg) :: history
        }
    , Command.Noop )
  | Event.Custom Acknowleged when role = Client ->
    let msg_body =
      Printf.sprintf "%s: %s" (Text_input.view text) (get_elapsed acknowleged)
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
  | Event.Custom Utils.Connected ->
    let model =
      if role = Client
      then
        Chat
          { model with
            connection = Open
          ; history =
              (Host, Printf.sprintf "Connected to host in %ss" (get_elapsed acknowleged))
              :: history
          }
      else
        Chat
          { model with
            connection = Open
          ; history = (Client, "Established connection with client") :: history
          }
    in
    model, Command.Noop
  | Event.Custom Closed -> Chat { model with connection = Shutdown }, Command.Noop
  | Event.KeyDown Event.Enter ->
    let model =
      (* Don't send multiple messages if a message is still pending *)
      if connection = Open && acknowleged = HostAcknowleged
      then (
        let msg = Text_input.view text in
        Riot.send_by_name ~name:Sender.name (Send msg);
        Chat { model with acknowleged = Sent (Unix.gettimeofday ()) })
      else Chat model
    in
    model, Command.Noop
  | Event.KeyDown Event.Escape -> Menu 0, Command.Hide_cursor
  | key ->
    Logger.error (fun f -> f "Event: %a" Event.pp key);
    let model =
      if acknowleged = HostAcknowleged
      then Chat { model with text = Text_input.update text key }
      else Chat model
    in
    model, Command.Noop
;;

let update event model =
  (match event with
   | Event.Custom _ as event ->
     Logger.error (fun f -> f "TUI RECEIVED: %a" Event.pp event)
   | _ -> ());
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
    List.mapi (fun i (choice, _, _, _) -> place_cursor i choice) menu
    |> String.concat "\n"
  in
  apply_list_style "Hi! What would you like to do?\n%s\nPress `esc` to exit" options
;;

let view_chat { role; connection; text; spinner; history; _ } =
  if Random.float 1. > 0.9999
  then
    Logger.error (fun f ->
      f "Current connection state in TUI: %s" (string_of_conn connection));
  let open Spices in
  let apply_role_style user (role, body) =
    let other_user_style = default |> fg other_user_color |> build in
    match user, role with
    | Host, Client -> other_user_style "Client: %!" ^ body
    | Host, Host | Client, Client ->
      let apply_user_style = Spices.(default |> fg info_color |> build) in
      apply_user_style "You: %!" ^ body
    | Client, Host -> other_user_style "Host: %!" ^ body
  in
  match role, connection with
  | _, Fatal message ->
    let apply_error_style = default |> fg error_color |> build in
    apply_error_style "Error: %s" message
  | Host, Waiting ->
    let sprite = Option.map Sprite.view spinner |> Option.value ~default:"" in
    info_style "Starting server and waiting for a connection %s" sprite
  | Client, Waiting ->
    let sprite = Option.map Sprite.view spinner |> Option.value ~default:"" in
    info_style "Connecting to server %s" sprite
  | _, Open ->
    let history = List.map (apply_role_style role) history |> String.concat "\n" in
    let history_syle = default |> margin_top 2 |> margin_left 2 |> build in
    let input_style = default |> margin_bottom 2 |> margin_left 2 |> build in
    let text_input = input_style "\n%s" @@ Text_input.view text in
    history_syle "%s\n" history ^ text_input
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
  Logger.set_log_level (Some Logger.Error);
  let pid = spawn_link run_tui in
  register tui_name pid;
  Ok pid
;;
