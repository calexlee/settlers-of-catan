
type t = Tile.t list

let initial_board  = 
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

(**[resource_list] is a list of all the possible resource in a board*)
let resource_list = 
  ["wood";"sheep";"wheat";"brick";"rock"; "brick"; "sheep"; "desert"; "wood"; 
   "wheat"; "wood"; "wheat"; "brick"; "sheep"; "sheep"; "rock"; "rock"; 
   "wheat"; "wood";]
(**[number] list is an ORDERED list of all the numbers on the board
   where its index represents its specific location*)
let number_list =
  [11;12;9;4;6;5;10;0;3;11;4;8;8;10;9;3;5;2;6]

(**[remove index start index lst] ist a [lst] without the element at [index]*)
let rec remove_index start index lst=
  match lst with
  |[]-> lst
  |h::t-> if start=index then remove_index (start+1) index t
    else h::(remove_index (start+1) index t)

(**[get_index start index lst] is the entry of [lst] at [index]*)
let rec get_index start index= function
  |[]-> failwith "index out of bounds"
  |h::t-> if start=index then h else get_index (start+1) index t

(**[random_resources acc lst] is [lst] but with its entries in a random order*)
let rec random_resources acc lst=
  match lst with 
  |[]->acc
  |x-> 
    let rand = Random.int (List.length lst) in
    random_resources ((get_index 0 rand lst)::acc) (remove_index 0 rand lst)
(**[rand_board_helper start num_lst rand_res_lst acc] is generates a 
   random board from [rand_res_lst] using the indexing of [num_lst]*)
let rec rand_board_helper start num_lst rand_res_lst acc= 
  match num_lst with
  |[]->acc
  |h_int::t_int-> 
    match rand_res_lst with
    |[]-> failwith "not enough resources"
    |h_res::t_res-> 
      let rob = h_res="desert" in
      rand_board_helper (start+1) t_int t_res ((Tile.make_tile h_int h_res rob)::acc)


let rand_board = 
  rand_board_helper 0 number_list (random_resources [] resource_list) []

let get_tile n t = 
  List.nth n t

let rec get_tiles_with_num board n = 
  match board with
  | [] -> []
  | h::t -> if ((Tile.get_number h) = n) then h :: get_tiles_with_num t n 
    else get_tiles_with_num t n

