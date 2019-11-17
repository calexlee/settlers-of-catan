(* Main to be implemented here *)
open Board
open Node

type phase = SETUP | PLAY | WIN

let player_list = [Player.make_player "green"; Player.make_player "magenta";
                   Player.make_player "yellow"; Player.make_player "blue"]

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

(* [rob_players] a function that runs through the players and removes
   half of their hand if they have more then 7 cards*)
let rob_players = 
  failwith "unimplemented"

(* *)

(*[play_game] a recrusive function that loops through the game playing where 
  [phase] represents the phase of the game [board] represents the board to 
  be drawn, [players] is the list of all the updated players and [turn] 
  is th INDEX OF THE PLAYER IN players whose turn it is *)
let rec play_game phase board players turn= 
  match phase with 
  |SETUP-> 
    Gamegraphics.draw_board board (generateNodes ()) ;
    print_endline("Welcome to settlers of catan! please decide who will be the 
  green player, magenta player, yellow player and blue player. If you are a new
  player enter help at any time to get instructions on commands, otherwise enter
  \"done\" to continue");
    let input = read_line() in 
    Gamegraphics.draw_board board (generateNodes ())
  (*green player place settlement and road*)
  (*magenta player place settlement and road*)
  (*yellow player place settlement and road *)
  (*blue player place settlement and road *)
  (*blue player place settlement and road *)
  (*yellow player place settlement and road *)
  (*magenta player place settlement and road*)
  (*green player place settlement and road*)
  (*give every player one resource from every tile touching their second settlement*)
  |PLAY->()

  (* 
    1. die roll 
    2. distribute recources
    3. wait for player input about building or done
    *)
  |WIN->()

let main () = 

  play_game SETUP (rand_board ()) player_list 0

(* Execute the game engine. *)
let () = main ()