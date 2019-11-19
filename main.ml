(* Main to be implemented here *)
open Board
open Node
open Lwt

(**[loop term] takes in a terminal and returns the row and column of a mouse
   click.
   Raises: "end" on a non=mouse click*)
let rec loop term =
  LTerm.read_event term
  >>= fun ev ->
  match ev with
  | LTerm_event.Mouse{ LTerm_mouse.row = row;LTerm_mouse.col = col; _ } ->
    return (row,col)
  | _ -> raise(Failure("end"))
(* loop term *)

(**[main] is starts the terminal mouse selection *)
let main () =
  Lwt_io.printl "Please Select a Node"
  >>= fun () ->
  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  LTerm.enter_raw_mode term
  >>= fun mode ->
  Lwt.finalize (fun () -> loop term)
    (fun () ->
       LTerm.leave_raw_mode term mode
       >>= fun () ->
       LTerm.disable_mouse term)

(**[selectNode] the index of the selected Node in the terminal
   Raises: "Not a Node" if the selection is not a node *)
let select_node () = let (row,col) = Lwt_main.run (main ()) in 
  Gamegraphics.rc_to_node (row,col)

(**[selectEdge] starts the selection process and prints the edge selected *)
let select_edge () = let (row,col) = Lwt_main.run (main ()) in 
  let (r,c) = (Gamegraphics.rc_to_edge (row,col)) in 
  print_string(string_of_int r);print_string(", ");
  print_endline(string_of_int c); ()

(**[printSelectNode l] takes in all clicks and prints out a list of the (row,col) 
   which were selected, on key press *)
let rec print_select_node l = try let (row,col) = Lwt_main.run (main ()) in 
    print_select_node 
      (String.concat "" ["(";(string_of_int row);",";(string_of_int col);")"]::l)
  with
  |Failure _ -> List.map print_endline (List.rev l)

type phase = Welcome | Setup | Win | Interactive | Roll | Help | Quit

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

(* [build_road turn board node] is a board but with the 
   players settlement built RAISES EXCEPTION IF PLAYER CAN NOT BUILD THERE
   condition for exception is that another player has a neighboring node*)
let build_road turn board node= 
  failwith "unimpelmented"

(* [build_settlement turn board node] is a board but with the 
   players settlement built RAISES EXCEPTION IF PLAYER CAN NOT BUILD THERE
   condition for exception is that another player has a neighboring node*)
let rec build_settlement turn nodes nodes_index counter acc= 
  (*BASE IMPLEMENTATION doers not check if the node is in the correct place*)
  match nodes with 
  |[]-> List.rev acc
  |h::t-> if (nodes_index=counter) then 
      (Node.add_settlement "settlement" (get_index 0 turn player_list) h);
    build_settlement turn t nodes_index (counter+1) (h::acc))
else build_settlement turn t nodes_index (counter+1) (h::acc);

(* [build_city turn board node] is a board but with the 
   players city built RAISES EXCEPTION IF PLAYER CAN NOT BUILD THERE
   Condition for excpetion is the player does not have a settlment there*)
let build_city turn board node= 
  failwith "unimpelmented" 

(* [rob_players] a function that runs through the players and removes
   half of their hand if they have more then 7 cards*)
let rec rob_players index = 
  if(index=4) then ()
  else Player.rob_player (get_index 0 index player_list);
  rob_players (index+1)

(*[give_resources nodes roll] is a recrusrive function that checks through 
  all of the nodes and gives all the players resources based on if they have
  a settlement there*)
let rec give_resources nodes roll = 
  match nodes with 
  |[]-> ()
  |h::t->Node.give_resource roll h;
    give_resources t roll;
    ()

(* [distrubute_resources players board roll] distributes the resources to the
   [players] according to the [board] and [roll] condition*)
let distribute_resources nodes roll = 
  if roll=7 then 
    rob_players 0
  else 
    give_resources nodes roll

(*[play_game] a recrusive function that loops through the game playing where 
  [phase] represents the phase of the game [board] represents the board to 
  be drawn, [players] is the list of all the updated players and [turn] 
  is th INDEX OF THE PLAYER IN players whose turn it is *)
let rec play_game phase prev_phase board nodes turn= 
  match phase with 
  |Welcome-> 
    ((Gamegraphics.draw_board board nodes);
     print_endline("Welcome to settlers of catan! please decide who will be the 
  green player, magenta player, yellow player and blue player. If you are a new
  player enter help at any time to get instructions on commands, otherwise enter
  \"done\" to continue");
     let input= Command.parse (read_line()) in
     ( match Command.to_string input with 
       |"help"->play_game Help Welcome board nodes turn
       |"done"->play_game Setup Welcome board nodes turn
       |"quit"->play_game Quit Welcome board nodes turn
       |_-> print_endline("Malformed command please re-enter");
         play_game Welcome Welcome board nodes turn;)
    )
  |Setup -> 
    (Gamegraphics.draw_board board nodes);
    print_endline("");
    (match turn with 
     |0->let node_index = select_node() in 
       Gamegraphics.draw_board board nodes
     |1->()


     |2->()


     |3->()
     |_ -> raise(Failure("not a player"));
    )
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

  |Help-> (
      (Gamegraphics.draw_board board (generate_nodes board));
      print_endline("\nHelp Menu: The valid commands include");
      print_endline("done - when you are done with your turn");
      print_endline("inventory - accsess your inventory");
      print_endline("add city - build a city when it is your turn, and you have required resources");
      print_endline("add settlement - build a settlement when it is your turn, and you have required resources");
      print_endline("help- display help menu");
      print_endline("quit- quit the game WARNING: progress will not be saved");
      let input= Command.parse (read_line()) in
      ( match Command.to_string input with 
        |"help"->play_game Help Help board nodes turn
        |"done"->play_game prev_phase Help board nodes turn
        |"quit"->play_game Quit Welcome board nodes turn
        |_-> print_endline("Malformed command please re-enter");
          play_game Help Help board nodes turn;)
    );

  |Roll->
    let die_roll = random_roll () in
    distribute_resources nodes die_roll;
    print_endline(
      "It is player " ^ Player.player_to_string (get_index 0 turn player_list)
      ^ " turn.");
    print_endline("The die roll resulted in a " ^ (string_of_int die_roll) ^
                  " and all of the resources have been distributed");
    play_game Interactive Roll board nodes turn 
  |Interactive-> ()
  |Win->()
  |Quit->print_endline("\nThank you for playing, all your progress has been lost");()

let main () = 
  let rand_board1 = rand_board () in
  play_game Welcome Welcome (rand_board1) (generate_nodes rand_board1) 0

(* Execute the game engine. *)
let () = main ()