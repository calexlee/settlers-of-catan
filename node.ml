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
  if counter=54 then acc 
  else addNodes ((make_node [] counter [])::acc) (counter+1)

let generateNodes () = 
  addNodes [] 0

let generateNodes2 board= 
  [make_node [board[1]] 0 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 1 [Edge.make_edge 1; Edge.make_edge ];
   make_node [board[1]] 2 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 3 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 4 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 5 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 6 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 7 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 8 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 9 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 10 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 11 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 12 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 13 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 14 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 15 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 16 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 17 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 18 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 19 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 20 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 21 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 22 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 23 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 24 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 25 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 26 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 27 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 28 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 29 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 30 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 31 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 32 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 33 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 34 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 35 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 36 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 37 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 38 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 39 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 41 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 42 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 43 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 44 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 45 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 46 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 47 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 48 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 49 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 50 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 51 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 52 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [board[1]] 53 [Edge.make_edge 1; Edge.make_edge 3];

  ]
