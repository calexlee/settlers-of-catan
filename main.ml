(* Main to be implemented here *)
open Board
open Node

type phase = SETUP | PLAY | WIN

(**[random_die] is a random die between 1 and 6*)
let random_die () = 
  (Random.int 6) + 1

(*[random_roll] generates a random number that corresponds to
  the sum of two random dies*)
let random_roll () = 
  let die1 = random_die () in 
  let die2 = random_die () in 
  die1 + die2


let main () = 
  let b = rand_board () in 
  Gamegraphics.draw_board b (generateNodes ())

(* Execute the game engine. *)
let () = main ()