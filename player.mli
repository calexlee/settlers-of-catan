(*  Consists of attributes of a list of resources, points, development card list and if this person has the longest road.*)

(** The abstract type of values representing player. *)
type t

(** The abstract type representing resources.*)
type r 

(**[get_points t] returns the points of player t.*)
val get_points : t -> int

(**[get_resources t] returns the resouces number list of player t.*)
val get_resources : t -> r list

(**[give_sheep t] gives the player t sheep.*)
val give_sheep : t -> t

(**[give_rock t] gives the player t rock.*)
val give_rock : t -> t

(**[give_ brick t] gives the player t brick.*)
val give_brick : t -> t

(**[give_sheep t] gives the player t wood.*)
val give_wood  : t -> t

(**[give_sheep t] gives the player t wheat.*)
val give_wheat : t -> t