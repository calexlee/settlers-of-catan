(* Main to be implemented here *)
open Board
open Node

type phase = SETUP | GREEN_PLAYER | MAGENTA_PLAYER | YELLOW_PLAYER | BLUE_PLAYER | WIN

let init = SETUP

(**[random_die] is a random die between 1 and 6*)
let random_die () = 
  (Random.int 6) + 1

(*[random_roll] generates a random number that corresponds to
  the sum of two random dies*)
let random_roll () = 
  let die1 = random_die () in 
  let die2 = random_die () in 
  die1 + die2


let rec play_game phase = 
  match phase with 
  |SETUP-> let b = rand_board () in 
    Gamegraphics.draw_board b (generateNodes ()) 
  (*green player place settlement and road*)
  (*magenta player place settlement and road*)
  (*yellow player place settlement and road *)
  (*blue player place settlement and road *)
  (*blue player place settlement and road *)
  (*yellow player place settlement and road *)
  (*magenta player place settlement and road*)
  (*green player place settlement and road*)
  |GREEN_PLAYER->()
  (* 
    1. die roll 
    2. distribute recources
    3. wait for player input
    *)
  |MAGENTA_PLAYER->()
  |YELLOW_PLAYER->()
  |BLUE_PLAYER->()
  |WIN->()

let main () = 

  play_game SETUP

(* Execute the game engine. *)
let () = main ()