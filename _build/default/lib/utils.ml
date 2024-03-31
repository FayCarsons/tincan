(** Message type that is sent between process so they may communicate *)
type Riot.Message.t +=
  | (* Start server (port) *)
      StartServer of int * Riot.Pid.t
  | (* Start client *)
      StartClient of Uri.t * Riot.Pid.t
  | (* Host/Client started *)
      Connected of Riot.Pid.t
  | (* Host sent back acknowleged message *)
      Acknowleged
  | (* Received a message *)
      Received of string
  | (* Send a message *)
      Send of string
  | (* Close connection and kill process *)
      Close
  | (* Connection closed *)
      Closed
  | (* Fatal error *)
      Err of string

let ( >> ) f g x = g @@ f x

type role =
  | Host
  | Client

let acknowleged = "$PING"
let chat_name = "Chat"
let tui_name = "tui"

let not_role = function
  | Host -> Client
  | Client -> Host
;;

let string_of_role = function
  | Host -> "Host"
  | Client -> "Client"
;;
