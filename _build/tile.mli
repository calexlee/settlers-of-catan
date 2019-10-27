(* Each tile has its own resources, number and if a robber sits on this tile.*)
module type Tile = sig

  (**[type r] will be a variant type that represents a tile*)
  type r 
  (**[type t] is a record that represents a tile, containing a resource,
     number of int and robber of bool*)
  type t 

end