type s = None | Settlement | City

type node_player = None | Some of Player.t

type t = {
  neigh_tiles: Tile.t list;
  mutable settlement : s;
  mutable player : node_player;
  index: int;
  edges: Edge.t list;
}

let make_node list n edge = 
  {
    neigh_tiles = list;
    settlement = None;
    player = None;
    index = n;
    edges = edge;
  }

let add_settlement name player t = 
  try 
    match name with
    |x when x="settlement"-> t.settlement <- Settlement; t.player <- Some player
    |x when x="city"-> t.settlement <- City; t.player <- Some player
    |_ -> failwith "invalid settlement type"
  with
  |Failure x -> ()

let remove_settlement t =
  t.settlement <- None; 
  t.player <- None

(**[give_resource_helper dr lst] fails if there are no tiles with that 
   die roll around a node. Otherwise it returns the resource from [lst] of
   tiles with number [dr] *)
let rec give_resource_helper (dr:int) (lst: Tile.t list)=
  match lst with 
  |[]-> failwith "none"
  |h::t-> if((Tile.get_number h) = dr) then (Tile.get_resource h)
    else give_resource_helper dr t

let give_resource (dr:int) (node:t)=
  match node.player with 
  |None -> failwith "No player"
  |Some n -> 
    try 
      match (give_resource_helper dr node.neigh_tiles) with
      |x when x="wood"->Player.give_wood n
      |x when x="sheep"->Player.give_sheep n
      |x when x="wheat"->Player.give_wheat n
      |x when x="rock"->Player.give_rock n
      |x when x="brick"->Player.give_brick n
      |_->failwith "invalid resource type"
    with
    |Failure x->()

let get_index t = 
  t.index

let get_tiles t = 
  t.neigh_tiles

let get_edges t = 
  t.edges

let get_settlement t =
  match t.settlement with 
  |None -> raise(Not_found)
  |Settlement -> "settlement"
  |City -> "city"

let get_player t =
  match t.player with
  |None -> raise(Not_found)
  |Some p -> p

let rec addNodes acc counter=
  if counter=55 then acc 
  else addNodes ((make_node [] counter [])::acc) (counter+1)

let generateNodes () = 
  addNodes [] 0



