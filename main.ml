(* Main to be implemented here *)
open Board
open Node

(**[get_index start index lst] is the entry of [lst] at [index]*)
let rec get_index start index= function
  |[]-> failwith "index out of bounds"
  |h::t-> if start=index then h else get_index (start+1) index t

(*[random_roll] generates a random number that corresponds to
  the sum of two random dies*)
let random_roll () = 
  let probability_list = 
    [2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,7,7,
     7,7,7,7,8,8,8,8,8,9,9,9,9,10,10,10,11,11,12] in 
  let index = Random.int 37 in
  get_index 0 index probability_list


let main () = 
  let b = rand_board () in 
  Gamegraphics.draw_board b (generateNodes ())

(* Execute the game engine. *)
let () = main ()