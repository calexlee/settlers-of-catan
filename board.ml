
type t = Tile.t list

let initial_board () = 
  [Tile.make_tile 11 "wood" false;
   Tile.make_tile 12 "sheep" false;
   Tile.make_tile 9 "wheat" false;
   Tile.make_tile 4 "brick" false;
   Tile.make_tile 6 "rock" false; 
   Tile.make_tile 5 "brick" false;
   Tile.make_tile 10 "sheep" false;
   Tile.make_tile 0 "desert" false;
   Tile.make_tile 3 "wood" false;
   Tile.make_tile 11 "wheat" false;
   Tile.make_tile 4 "wood" false;
   Tile.make_tile 8 "wheat" false;
   Tile.make_tile 8 "brick" false;
   Tile.make_tile 10 "sheep" false;
   Tile.make_tile 9 "sheep" false; 
   Tile.make_tile 3 "rock" false;
   Tile.make_tile 5 "rock" false;
   Tile.make_tile 2 "wheat" false;
   Tile.make_tile 6 "wood" false;
  ]
let rand_board = 
  failwith "unimpelmented"

let get_tile n t = 
  List.nth n t

let rec get_tiles_with_num board n = 
  match board with
  | [] -> []
  | h::t -> if ((Tile.get_number h) = n) then h :: get_tiles_with_num t n 
    else get_tiles_with_num t n

