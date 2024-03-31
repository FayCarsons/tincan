(** Message type processes use to communicate *)
type Riot.Message.t +=
  | StartServer of int * Riot.Pid.t  (** Start server *)
  | StartClient of Uri.t * Riot.Pid.t  (** Start client  *)
  | Connected of Riot.Pid.t  (** Host/Client started *)
  | Acknowleged  (** Host sent back acknowleged message *)
  | Received of string  (** Received a message *)
  | Send of string  (** Send a message *)
  | Close  (** Close connection and kill process *)
  | Closed (* Connection closed *)
  | Err of string  (** Server error *)

let ( >> ) f g x = g @@ f x

(** The user's role *)
type role = Host | Client

let acknowleged = "$PING"
