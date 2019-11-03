(*  Consists of attributes of a list of resources, points, development card list and if this person has the longest road.*)

(** The abstract type of values representing player. *)
type t

(** The abstract type representing resources.*)
type r

(**[make_player] returns a new player t.*)
val make_player : t

(**[get_points t] returns the points of player t.*)
val get_points : t -> int

(**[get_resources t] returns the resouces number list of player t.*)
val get_resources : t -> r list

(**[give_sheep t] gives the player t sheep.*)
val give_sheep : t -> unit

(**[give_rock t] gives the player t rock.*)
val give_rock : t -> unit

(**[give_ brick t] gives the player t brick.*)
val give_brick : t -> unit

(**[give_sheep t] gives the player t wood.*)
val give_wood  : t -> unit

(**[give_sheep t] gives the player t wheat.*)
val give_wheat : t -> unit