module Node = struct 

  type s = None | Settlement | City

  type t = {
    neigh_Tiles: Tile.t list;
    settlement : s;
    (**Player : player.player *) 
    index: int;
    edges: Edge.t list;
  }

  let give_resource dr =
    failwith("Unimplemented")



end