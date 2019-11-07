(* Main to be implemented here *)
open Board
open Node

let main () = 
  let b = rand_board () in 
  Graphics.draw_board b (generateNodes ())

(* Execute the game engine. *)
let () = main ()