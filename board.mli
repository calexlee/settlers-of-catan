

(**[type t] represents a list of all of the tiles in the board*)
(**RI: the index of the list represents what tile it is**)
type t

(**[initial_board] becomes a pre-built standard board of type t*)
val initial_board : unit->t

(**[rand_board] becomes a randomized board of type t*)
val rand_board :unit->t

(**[get_tile] is a the type located at an index*)
val get_tile : int -> Tile.t

(**[get_tiles_with_num] is the list of tiles with a certain int *)
val get_tiles_with_num : int -> Tile.t list
