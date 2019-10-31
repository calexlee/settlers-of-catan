type s = None | Settlement | City

type t = {
  neigh_tiles: Tile.t list;
  settlement : s;
  player : Player.player;
  index: int;
  edges: Edge.t list;
}

(**[give_resource_helper dr lst] fails if there are no tiles with that 
   die roll around a node. Otherwise it returns the resource from [lst] of
   tiles with number [dr] *)
let rec give_resource_helper (dr:int) (lst: Tile.t list)=
  match lst with 
  |[]-> failwith "none"
  |h::t-> if(h.number = dr) then t.resource else give_resource_helper dr t

let give_resource (dr:int) (node:Node.t)=
  try 
    match (give_resource_helper dr node.neigh_tiles) with
    |Wood->Player.give_wood t.player
    |Sheep->Player.give_sheep t.player
    |Wheat->Player.give_wheat t.player
    |Rock->Player.give_rock t.player
    |Brick->Plater.give_brick t.player
  with
  |Failure->()


