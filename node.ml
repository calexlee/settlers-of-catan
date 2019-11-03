type s = None | Settlement | City

type t = {
  neigh_tiles: Tile.t list;
  settlement : s;
  player : Player.t;
  index: int;
  edges: Edge.t list;
}

(**[give_resource_helper dr lst] fails if there are no tiles with that 
   die roll around a node. Otherwise it returns the resource from [lst] of
   tiles with number [dr] *)
let rec give_resource_helper (dr:int) (lst: Tile.t list)=
  match lst with 
  |[]-> failwith "none"
  |h::t-> if((Tile.get_number h) = dr) then (Tile.get_resource h)
    else give_resource_helper dr t

let give_resource (dr:int) (node:t)=
  try 
    match (give_resource_helper dr node.neigh_tiles) with
    |x when x="wood"->Player.give_wood node.player
    |x when x="sheep"->Player.give_sheep node.player
    |x when x="wheat"->Player.give_wheat node.player
    |x when x="rock"->Player.give_rock node.player
    |x when x="brick"->Player.give_brick node.player
    |_->failwith "invalid resource type"
  with
  |Failure x->()

let get_index t = 
  t.index
