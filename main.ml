(* Main to be implemented here *)
open Board
open Node

type phase = Setup | Win | Interactive | Roll

let player_list = [Player.make_player "green"; Player.make_player "magenta";
                   Player.make_player "yellow"; Player.make_player "blue"]

(**[random_die] is a random die between 1 and 6*)
let random_die () = 
  (Random.int 6) + 1

(*[random_roll] generates a random number that corresponds to
  the sum of two random dies*)
let random_roll () = 
  let die1 = random_die () in 
  let die2 = random_die () in 
  die1 + die2

(**[get_index start index lst] is the entry of [lst] at [index]*)
let rec get_index start index= function
  |[]-> failwith "index out of bounds"
  |h::t-> if start=index then h else get_index (start+1) index t

(* [rob_players] a function that runs through the players and removes
   half of their hand if they have more then 7 cards*)
let rob_players players= 
  failwith "unimplemented"

(* [build_settlement players turn board node] is a board but with the 
   players settlement built RAISES EXCEPTION IF PLAYER CAN NOT BUILD THERE
   condition for exception is that another player has a neighboring node*)
let build_settlement players turn board node= 
  failwith "unimpelmented"

(* [build_city players turn board node] is a board but with the 
   players city built RAISES EXCEPTION IF PLAYER CAN NOT BUILD THERE
   Condition for excpetion is the player does not have a settlment there*)
let build_settlement players turn board node= 
  failwith "unimpelmented" 

(* [distrubute_resources players board roll] distributes the resources to the
   [players] according to the [board] and [roll] condition*)
let distribute_resources players board roll = 
  if roll=7 then 
    rob_players players
  else
    let tile_list = Board.get_tiles_with_num board roll in 
    failwith "unimplemented"

(*[play_game] a recrusive function that loops through the game playing where 
  [phase] represents the phase of the game [board] represents the board to 
  be drawn, [players] is the list of all the updated players and [turn] 
  is th INDEX OF THE PLAYER IN players whose turn it is *)
let rec play_game phase board players turn= 
  match phase with 
  |Setup-> 
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

  (* 
    1. die roll 
    2. distribute recources
    3. wait for player input about building or done
    *)
  |Roll->
    let die_roll = random_roll () in
    let updated_players = distribute_resources players board die_roll in
    print_endline(
      "It is player " ^ Player.player_to_string (get_index 0 turn player_list)
      ^ " turn.");
    print_endline("The die roll resulted in a " ^ (string_of_int die_roll) ^
                  " and all of the resources have been distributed");
    play_game Interactive board updated_players turn
  |Interactive->()
  |Win->()

let main () = 

  play_game Setup (rand_board ()) player_list 0

(* Execute the game engine. *)
let () = main ()