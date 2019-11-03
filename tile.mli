(**[type r] will be a variant type that represents a tile*)
type r 
(**[type t] is a record that represents a tile, containing a resource,
   number of int and robber of bool*)
type t 


(**[make_tile number resource] is a tile with the number [number] and 
   with the resource [resource]*)
val make_tile : int -> string -> bool -> t

(**[get_reosurce tile] is the resource of tile [tile]*)
val get_resource : t -> string

(**[get_number tile] is the number of tile [tile]*)
val get_number : t -> int
