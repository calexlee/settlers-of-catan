module type Node = sig 

  (**The type of a settlement *)
  type s

  (**The type of a Node *)
  type t = {
    neigh_Tiles: Tile.t list;
    settlement : s;
    (**Player : player.player *) 
    index: int;
    edges: Edge.t list;
  }

  (**[give_resource dr] changes the resources of the players who have
     settlements adjacent to the tiles with the number [dr]*)
  val give_resource : int -> unit


end