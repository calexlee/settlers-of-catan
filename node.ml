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

let rec add_nodes acc counter=
  if counter=54 then acc 
  else add_nodes ((make_node [] counter [])::acc) (counter+1)

let generate_empty_nodes () = 
  add_nodes [] 0

(**[find_edges elist e] is the player at position [e] in [elist] or None *)
let rec find_edges elist e = 
  match elist with 
  | [] -> raise(Failure("No edge at position"))
  | h::t -> if (Edge.get_index h) == e 
    then Edge.get_player h 
    else 
      find_edges t e

let has_edge node = function 
  |(x,y) -> find_edges (get_edges node) y

let generate_nodes board= 
  [make_node [Board.get_tile board 0] 0 [Edge.make_edge 1; Edge.make_edge 3];
   make_node [Board.get_tile board 0] 1 [Edge.make_edge 0; Edge.make_edge 4];
   make_node [Board.get_tile board 1] 2 [Edge.make_edge 3; Edge.make_edge 7];
   make_node [Board.get_tile board 0; Board.get_tile board 1] 3 [Edge.make_edge 0; Edge.make_edge 2; Edge.make_edge 8];
   make_node [Board.get_tile board 0; Board.get_tile board 2] 4 [Edge.make_edge 1; Edge.make_edge 5; Edge.make_edge 9];
   make_node [Board.get_tile board 2] 5 [Edge.make_edge 4; Edge.make_edge 10];
   make_node [Board.get_tile board 3] 6 [Edge.make_edge 7; Edge.make_edge 12];
   make_node [Board.get_tile board 1; Board.get_tile board 3] 7 [Edge.make_edge 2; Edge.make_edge 6; Edge.make_edge 13];
   make_node [Board.get_tile board 0; Board.get_tile board 1; Board.get_tile board 4] 8 [Edge.make_edge 3; Edge.make_edge 9; Edge.make_edge 14];
   make_node [Board.get_tile board 0; Board.get_tile board 2; Board.get_tile board 4] 9 [Edge.make_edge 4; Edge.make_edge 8; Edge.make_edge 15];
   make_node [Board.get_tile board 2; Board.get_tile board 5] 10 [Edge.make_edge 5; Edge.make_edge 11; Edge.make_edge 16];
   make_node [Board.get_tile board 5] 11 [Edge.make_edge 10; Edge.make_edge 17];
   make_node [Board.get_tile board 3] 12 [Edge.make_edge 6; Edge.make_edge 18];
   make_node [Board.get_tile board 1; Board.get_tile board 3; Board.get_tile board 6] 13 [Edge.make_edge 7; Edge.make_edge 14; Edge.make_edge 19];
   make_node [Board.get_tile board 1; Board.get_tile board 4; Board.get_tile board 6] 14 [Edge.make_edge 8; Edge.make_edge 13; Edge.make_edge 20];
   make_node [Board.get_tile board 2; Board.get_tile board 4; Board.get_tile board 7] 15 [Edge.make_edge 9; Edge.make_edge 16; Edge.make_edge 21];
   make_node [Board.get_tile board 2; Board.get_tile board 5; Board.get_tile board 7] 16 [Edge.make_edge 10; Edge.make_edge 15; Edge.make_edge 22];
   make_node [Board.get_tile board 5] 17 [Edge.make_edge 11; Edge.make_edge 23];
   make_node [Board.get_tile board 3; Board.get_tile board 8] 18 [Edge.make_edge 12; Edge.make_edge 19; Edge.make_edge 24];
   make_node [Board.get_tile board 3; Board.get_tile board 6; Board.get_tile board 8] 19 [Edge.make_edge 13; Edge.make_edge 18; Edge.make_edge 25];
   make_node [Board.get_tile board 4; Board.get_tile board 6; Board.get_tile board 9] 20 [Edge.make_edge 14; Edge.make_edge 21; Edge.make_edge 26];
   make_node [Board.get_tile board 4; Board.get_tile board 7; Board.get_tile board 9] 21 [Edge.make_edge 15; Edge.make_edge 20; Edge.make_edge 27];
   make_node [Board.get_tile board 5; Board.get_tile board 7; Board.get_tile board 10] 22 [Edge.make_edge 16; Edge.make_edge 23; Edge.make_edge 28];
   make_node [Board.get_tile board 5; Board.get_tile board 10] 23 [Edge.make_edge 17; Edge.make_edge 22; Edge.make_edge 29];
   make_node [Board.get_tile board 8] 24 [Edge.make_edge 18; Edge.make_edge 30];
   make_node [Board.get_tile board 6; Board.get_tile board 8; Board.get_tile board 11] 25 [Edge.make_edge 19; Edge.make_edge 26; Edge.make_edge 31];
   make_node [Board.get_tile board 6; Board.get_tile board 9; Board.get_tile board 11] 26 [Edge.make_edge 20; Edge.make_edge 25; Edge.make_edge 32];
   make_node [Board.get_tile board 7; Board.get_tile board 9; Board.get_tile board 12] 27 [Edge.make_edge 21; Edge.make_edge 28; Edge.make_edge 33];
   make_node [Board.get_tile board 7; Board.get_tile board 10; Board.get_tile board 12] 28 [Edge.make_edge 22; Edge.make_edge 27; Edge.make_edge 34];
   make_node [Board.get_tile board 10] 29 [Edge.make_edge 23; Edge.make_edge 35];
   make_node [Board.get_tile board 8; Board.get_tile board 13] 30 [Edge.make_edge 24; Edge.make_edge 36];
   make_node [Board.get_tile board 8; Board.get_tile board 11; Board.get_tile board 13] 31 [Edge.make_edge 25; Edge.make_edge 30; Edge.make_edge 37];
   make_node [Board.get_tile board 9; Board.get_tile board 11; Board.get_tile board 14] 32 [Edge.make_edge 26; Edge.make_edge 33; Edge.make_edge 38];
   make_node [Board.get_tile board 9; Board.get_tile board 12; Board.get_tile board 14] 33 [Edge.make_edge 27; Edge.make_edge 32; Edge.make_edge 39];
   make_node [Board.get_tile board 10; Board.get_tile board 12; Board.get_tile board 15] 34 [Edge.make_edge 28; Edge.make_edge 35; Edge.make_edge 40];
   make_node [Board.get_tile board 10; Board.get_tile board 15] 35 [Edge.make_edge 29; Edge.make_edge 41];
   make_node [Board.get_tile board 13] 36 [Edge.make_edge 30; Edge.make_edge 42];
   make_node [Board.get_tile board 11; Board.get_tile board 13; Board.get_tile board 16] 37 [Edge.make_edge 31; Edge.make_edge 38; Edge.make_edge 43];
   make_node [Board.get_tile board 11; Board.get_tile board 14; Board.get_tile board 16] 38 [Edge.make_edge 32; Edge.make_edge 37; Edge.make_edge 44];
   make_node [Board.get_tile board 12; Board.get_tile board 14; Board.get_tile board 17] 39 [Edge.make_edge 33; Edge.make_edge 40; Edge.make_edge 45];
   make_node [Board.get_tile board 12; Board.get_tile board 15; Board.get_tile board 17] 40 [Edge.make_edge 34; Edge.make_edge 39; Edge.make_edge 46];
   make_node [Board.get_tile board 15] 41 [Edge.make_edge 35; Edge.make_edge 47];
   make_node [Board.get_tile board 13] 42 [Edge.make_edge 36; Edge.make_edge 43];
   make_node [Board.get_tile board 13; Board.get_tile board 16] 43 [Edge.make_edge 37; Edge.make_edge 42; Edge.make_edge 48];
   make_node [Board.get_tile board 14; Board.get_tile board 16; Board.get_tile board 18] 44 [Edge.make_edge 38; Edge.make_edge 45; Edge.make_edge 49];
   make_node [Board.get_tile board 14; Board.get_tile board 17; Board.get_tile board 18] 45 [Edge.make_edge 39; Edge.make_edge 44; Edge.make_edge 50];
   make_node [Board.get_tile board 15; Board.get_tile board 17] 46 [Edge.make_edge 40; Edge.make_edge 47; Edge.make_edge 51];
   make_node [Board.get_tile board 15] 47 [Edge.make_edge 41; Edge.make_edge 46];
   make_node [Board.get_tile board 16] 48 [Edge.make_edge 43; Edge.make_edge 49];
   make_node [Board.get_tile board 16; Board.get_tile board 18] 49 [Edge.make_edge 44; Edge.make_edge 48; Edge.make_edge 52];
   make_node [Board.get_tile board 17; Board.get_tile board 18] 50 [Edge.make_edge 45; Edge.make_edge 51; Edge.make_edge 53];
   make_node [Board.get_tile board 17] 51 [Edge.make_edge 46; Edge.make_edge 50];
   make_node [Board.get_tile board 18] 52 [Edge.make_edge 49; Edge.make_edge 53];
   make_node [Board.get_tile board 18] 53 [Edge.make_edge 50; Edge.make_edge 52];
  ]
