
type t = Tile.t list


let initial_board = 
  failwith "unimpelmented"

let rand_board = 
  failwith "unimpelmented"

let get_tile n t = 
  List.nth n t

let get_tiles_with_num board n = 
  match board with
  | [] -> []
  | h::t -> if (h.number = n) then h :: get_tiles_with_num t n 
  else get_tiles_with_num t n

