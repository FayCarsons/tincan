type strategy =
  | SingleProcess
  | Multiprocess

let current_strategy = SingleProcess

type Riot.Message.t +=
  | Alive
  | (* Start server (port) *)
      StartServer of int * Riot.Pid.t
  | (* Start client *)
      StartClient of Uri.t * Riot.Pid.t
  | (* Host/Client started *) Connected
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
  | Err of string

let ( >> ) f g x = g @@ f x

type role =
  | Host
  | Client

let acknowleged = "$ACKNOWLEGED"
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
