(*  Consists of attributes of a list of resources, points, development card list and if this person has the longest road.*)

(** The abstract type of values representing player. *)
type t

(** The abstract type representing resources.*)
type r

(** The abstract type representing color*)
type color

(*[make_player color] creates a new player with color [color] *)
val make_player : string -> t

(**[get_points t] returns the points of player [t].*)
val get_points : t -> int

(**[get_color t] is the color of player [t]  *)
val get_color : t -> color

(**[color_to_string c] is the string representing [c] *)
val color_to_string : color -> string

(**[get_resources t] returns the resouces number list of player [t]*)
val get_resources : t -> r list

(**[give_sheep t] gives the player [t] sheep.*)
val give_sheep : t -> unit

(**[give_rock t] gives the player [t] rock.*)
val give_rock : t -> unit

(**[give_ brick t] gives the player [t] brick.*)
val give_brick : t -> unit

(**[give_sheep t] gives the player [t] wood.*)
val give_wood  : t -> unit

(**[give_sheep t] gives the player [t] wheat.*)
val give_wheat : t -> unit

(**[player_to_string player] is a string representation of [player]*)
val player_to_string : t -> string

(**[resources_to_string player] is a string representation of [player.resources]*)
val resources_to_string : t -> string list

(**[rob_player t] mutates player [t] so that they lose half of their resources*)
val rob_player : t -> unit

(**[build_settlement player] checks if [player] has enough resources to build
   a settlement and if they do, then removes those resources from the players
   inventory*)
val build_settlement: t->unit

(**[build_city player] checks if [player] has enough resources to build
   a city and if they do, then removes those resources from the players
   inventory*)
val build_city: t->unit


(**[build_road player] checks if [player] has enough resources to build a road,
   if they do then it removes those resources from the players inventory*)
val build_road: t-> unit

