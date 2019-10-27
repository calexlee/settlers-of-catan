type s = None | Settlement | City

type t = {
  neigh_tiles: Tile.t list;
  settlement : s;
  (**Player : player.player *) 
  index: int;
  edges: Edge.t list;
}

let give_resource dr =
  failwith("Unimplemented")


