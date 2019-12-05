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
  Lwt_io.printl ""
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

(**[select_node] the index of the selected Node in the terminal
   Raises: "Not a Node" if the selection is not a node *)
let select_node () = let (row,col) = Lwt_main.run (main ()) in
  Gamegraphics.rc_to_node (row,col)

(**[select_edge] is the pair of nodes connecting the edge selected *)
let select_edge () = let (row,col) = Lwt_main.run (main ()) in
  (Gamegraphics.rc_to_edge (row,col))

(**[select_tile] is the index of the selected tile  *)
let select_tile () = let (row,col) = Lwt_main.run (main ()) in
  (Gamegraphics.rc_to_tile (row,col))

(**[print_select_node l] takes in all clicks and prints out a list of the (row,col)
   which were selected, on key press *)
let rec print_select_node l = try let (row,col) = Lwt_main.run (main ()) in
    print_select_node
      (String.concat "" ["(";(string_of_int row);",";(string_of_int col);")"]::l)
  with
  |Failure _ -> ignore(List.map print_endline (List.rev l));()

type phase = Welcome | Setup | Win | Interactive | Roll | Help | Quit |Inventory
           | Points | AddSettle | AddCity | AddRoad | Robbing | BuyCard | Cards
           | UseKnight | UseProgress | UseVictory | AddFreeRoad

let player_list = [Player.make_player "green"; Player.make_player "magenta";
                   Player.make_player "yellow"; Player.make_player "blue"]

(**[random_die] is a random die between 1 and 6*)
let random_die () =
  Random.self_init ();
  (Random.int 6) + 1

(** [random_card list] generates a random development card*)
let random_card (list:Player.card list) : Player.card = 
  Random.self_init ();
  List.nth list (Random.int (List.length list))

(** [delete_card card list] deletes this card from list*)
let delete_card (card:Player.card) (list:Player.card list) = 
  let rec res card list ret = 
    match list with
    | [] -> ret
    | h :: t -> if h = card then ret @ t else res card t (h::ret) in res card list []

(**[add_node] returns a list of nodes that have been selected by players*)
let add_node list = function
  | 0 -> 0::1::3::list
  | 1 -> 0::1::4::list
  | 2 -> 2::3::7::list
  | 3 -> 0::2::3::8::list
  | 4 -> 1::4::5::9::list
  | 5 -> 4::5::10::list
  | 6 -> 6::7::12::list
  | 7 -> 2::6::7::13::list
  | 8 -> 3::8::9::14::list
  | 9 -> 4::8::9::15::list
  | 10 -> 5::10::11::16::list
  | 11 -> 10::11::17::list
  | 12 -> 6::12::18::list
  | 13 -> 7::13::14::19::list
  | 14 -> 8::13::14::20::list
  | 15 -> 9::15::16::21::list
  | 16 -> 10::15::16::22::list
  | 17 -> 11::17::23::list
  | 18 -> 12::18::19::24::list
  | 19 -> 13::18::19::25::list
  | 20 -> 14::20::21::26::list
  | 21 -> 15::20::21::27::list
  | 22 -> 16::22::23::28::list
  | 23 -> 17::22::23::list
  | 24 -> 18::24::30::list
  | 25 -> 19::25::26::31::list
  | 26 -> 20::25::26::32::list
  | 27 -> 21::27::28::33::list
  | 28 -> 22::27::28::34::list
  | 29 -> 23::29::35::list
  | 30 -> 24::30::36::list
  | 31 -> 25::30::31::37::list
  | 32 -> 26::32::33::38::list
  | 33 -> 27::32::33::39::list
  | 34 -> 28::34::35::40::list
  | 35 -> 29::34::35::41::list
  | 36 -> 30::36::42::list
  | 37 -> 31::37::38::43::list
  | 38 -> 32::37::38::44::list
  | 39 -> 33::39::40::45::list
  | 40 -> 34::39::40::46::list
  | 41 -> 35::41::47::list
  | 42 -> 36::42::43::list
  | 43 -> 37::42::43::48::list
  | 44 -> 38::44::45::49::list
  | 45 -> 39::44::45::50::list
  | 46 -> 40::46::47::51::list
  | 47 -> 41::46::47::list
  | 48 -> 43::48::49::list
  | 49 -> 44::48::49::52::list
  | 50 -> 45::50::51::53::list
  | 51 -> 46::50::51::list
  | 52 -> 49::52::53::list
  | 53 -> 50::52::53::list
  | _ -> failwith "not a node number"

(** [have_road] checks if nodes are connected by roads*)
let rec have_road turn n e_list =
  match e_list with
  | [] -> false
  | (a, b, c) :: t -> if turn = a then
      begin
        if (n = b || n = c) then true else have_road turn n t
      end
    else have_road turn n t

(**[get_turn_nodes] get all the nodes belonging to this player that have been selected*)
let rec get_turn_nodes turn ret = function
  | [] -> ret
  | (a, n, -1) :: t -> if a=turn then get_turn_nodes turn (n::ret) t else get_turn_nodes turn ret t
  | _ :: t -> get_turn_nodes turn ret t

(**[if_neighbor] checks if nodes can be selected by players*)
let if_neighbor phase turn n n_list e_list =
  match phase with
  | 0 -> List.mem n n_list
  | 1 -> if List.mem n n_list then true else not (have_road turn n e_list)
  | _ -> failwith "not a valid phase"

(**[if_city] checks if we can build city here*)
let if_city turn n e_list =
  List.mem n (get_turn_nodes turn [] e_list)

(**[if_edge] checks if this edge is conneted with this node*)
let rec if_edge turn edge list =
  if (List.mem (0, fst edge, snd edge) list ||
      List.mem (0, snd edge, fst edge) list ||
      List.mem (1, fst edge, snd edge) list ||
      List.mem (1, snd edge, fst edge) list ||
      List.mem (2, fst edge, snd edge) list ||
      List.mem (2, snd edge, fst edge) list ||
      List.mem (3, fst edge, snd edge) list ||
      List.mem (3, snd edge, fst edge) list ) then false else
    begin
      match list with
      | [] -> false
      | (a, b, c) :: t -> if turn = a then
          begin
            if (fst edge = b || fst edge = c || snd edge = b || snd edge = c) then true else if_edge turn edge t
          end
        else if_edge turn edge t
    end

(**[random_roll] generates a random number that corresponds to
   the sum of two random dies*)
let random_roll () =
  let die1 = random_die () in
  let die2 = random_die () in
  die1 + die2

(**[get_index start index lst] is the entry of [lst] at [index]*)
let rec get_index start index= function
  |[]-> failwith "index out of bounds"
  |h::t-> if start=index then h else get_index (start+1) index t


(**[build_road turn board node] is a board but with the
   players settlement built RAISES EXCEPTION IF PLAYER CAN NOT BUILD THERE
   condition for exception is that another player has a neighboring node*)
let rec build_road_helper turn nodes node1 node2 counter acc =
  match nodes with
  |[]-> List.rev acc
  |h::t-> if (node1=counter) then (
      let edge = Node.get_edge node2 h in
      Edge.add_road edge (Some (get_index 0 turn player_list));
      build_road_helper turn t node1 node2 (counter+1) (h::acc))
    else if (node2=counter) then (
      let edge = Node.get_edge node1 h in
      Edge.add_road edge (Some (get_index 0 turn player_list));
      build_road_helper turn t node1 node2 (counter+1) (h::acc))
    else build_road_helper turn t node1 node2 (counter+1) (h::acc)

let build_road turn nodes node_tup counter acc =
  match node_tup with
  |(x,y)-> build_road_helper turn nodes x y counter acc

(**[build_settlement turn board node] is a board but with the
   players settlement built RAISES EXCEPTION IF PLAYER CAN NOT BUILD THERE
   condition for exception is that another player has a neighboring node*)
let rec build_settlement turn nodes nodes_index counter acc building=
  match nodes with
  |[]-> List.rev acc
  |h::t-> if (nodes_index=counter) then (
      (Node.add_settlement building (get_index 0 turn player_list) h);
      build_settlement turn t nodes_index (counter+1) (h::acc) building)
    else build_settlement turn t nodes_index (counter+1) (h::acc) building

(**[start_resources turn nodes nodes_index counter] is a function that returns
   unit after giving a player all the resources that they start with*)
let rec start_resources turn nodes nodes_index counter =
  match nodes with
  |[]-> ()
  |h::t-> if nodes_index=counter then failwith ""
    else start_resources turn nodes nodes_index (counter+1)

(**[rob_players] a function that runs through the players and removes
   half of their hand if they have more then 7 cards*)
let rob_players () =
  Player.rob_player (get_index 0 0 player_list);
  Player.rob_player (get_index 0 1 player_list);
  Player.rob_player (get_index 0 2 player_list);
  Player.rob_player (get_index 0 3 player_list);
  ()
(**[give_resources nodes roll] is a recrusrive function that checks through
   all of the nodes and gives all the players resources based on if they have
   a settlement there*)
let rec give_resources nodes roll =
  (match nodes with
   |[]-> ()
   |h::t->
     try (Node.give_resource roll h;
          give_resources t roll)
     with |_-> give_resources t roll)

(**[distrubute_resources players board roll] distributes the resources to the
    [players] according to the [board] and [roll] condition*)
let distribute_resources nodes roll =
  if roll=7 then
    rob_players ()
  else
    give_resources nodes roll

(**[give_port node_index nodes turn] gives the player corresponding to [turn]
   if the node_index has a a port on it *)
let give_port node_index nodes turn=
  try (
    if Node.has_three_to_one (get_index 0 node_index nodes) then 
      (Player.give_port (get_index 0 turn player_list) true "")
    else if Node.has_res_port (get_index 0 node_index nodes)!="" then 
      (Player.give_port (get_index 0 turn player_list) false  
         (Node.has_res_port (get_index 0 node_index nodes)))
    else ()) with |_->()


(**[play_game] a recursive function that loops through the game playing where
   [phase] represents the phase of the game [board] represents the board to
   be drawn, [players] is the list of all the updated players and [turn]
   is the INDEX OF THE PLAYER IN players whose turn it is *)
let rec play_game phase prev_phase board nodes turn pass rd_ph list node message card_list=
  match phase with
  |Welcome->
    print_endline(message);
    ((Gamegraphics.draw_board board nodes);
     print_endline("Welcome to settlers of catan! please decide who will be the
  green player, magenta player, yellow player and blue player. If you are a new
  player enter help at any time to get instructions on commands, otherwise enter
  \"done\" to continue");
     let input= Command.parse (read_line()) in
     ( match Command.to_data input with
       |("help",_,_,_,_)->play_game Help Welcome board nodes turn pass false list node message card_list
       |("done",_,_,_,_)->play_game Setup Welcome board nodes turn pass false list node message card_list
       |("quit",_,_,_,_)->play_game Quit Welcome board nodes turn pass false list node message card_list
       |_-> let msg = "Malformed command please re-enter" in
         play_game Welcome Welcome board nodes turn pass false list node msg card_list;)
    )
  |Setup ->
    (try
       (match turn with
        |0->
          if(not rd_ph) then
            begin
              print_endline("Green player, please select a node to build a settlement");
              let node_index =  select_node() in
              if (if_neighbor 0 0 node_index list node) then failwith "wrong position" else
                begin 
                  give_port node_index nodes turn;
                  Gamegraphics.draw_board board (build_settlement turn nodes node_index 0 [] "settlement");
                  if pass then Node.give_resource_start (get_index 0 node_index nodes) else ();
                  play_game Setup Setup board nodes turn pass true (add_node list node_index) ((turn, node_index, -1)::node) message card_list;
                end
            end
          else
            begin
              print_endline("Green Player, please select an edge next to your settlement to place a road");
              let selected_edge = select_edge () in
              if (not (if_edge turn selected_edge node)) then failwith "wrong position" else
                begin
                  Gamegraphics.draw_board board (build_road turn nodes selected_edge 0 []);
                  if (pass)then play_game Roll Setup board nodes turn pass false list ((turn, fst selected_edge, snd selected_edge)::node) message card_list
                  else play_game Setup Setup board nodes (turn+1) pass false list ((turn, fst selected_edge, snd selected_edge)::node) message card_list
                end
            end
        |1->
          if(not rd_ph) then
            begin
              print_endline("Magenta player, please select a node to build a settlement");
              let node_index =  select_node()  in
              if (if_neighbor 0 1 node_index list node) then failwith "wrong position" else
                begin
                  give_port node_index nodes turn;
                  Gamegraphics.draw_board board (build_settlement turn nodes node_index 0 [] "settlement");
                  if pass then Node.give_resource_start (get_index 0 node_index nodes) else ();
                  play_game Setup Setup board nodes turn pass true (add_node list node_index) ((turn, node_index, -1)::node) message card_list;
                end
            end
          else
            begin
              print_endline("Magenta Player, please select an edge next to your settlement to place a road");
              let selected_edge = select_edge () in
              if (not (if_edge turn selected_edge node)) then failwith "wrong position" else
                begin
                  Gamegraphics.draw_board board (build_road turn nodes selected_edge 0 []);
                  if (pass) then play_game Setup Setup board nodes (turn-1) pass false list ((turn, fst selected_edge, snd selected_edge)::node) message card_list
                  else play_game Setup Setup board nodes (turn+1) pass false list ((turn, fst selected_edge, snd selected_edge)::node) message card_list
                end
            end
        |2->
          if(not rd_ph) then
            begin
              print_endline("Yellow player, please select a node to build a settlement.");
              let node_index =  select_node()  in
              if (if_neighbor 0 2 node_index list node) then failwith "wrong position" else
                begin
                  give_port node_index nodes turn;
                  Gamegraphics.draw_board board (build_settlement turn nodes node_index 0 [] "settlement");
                  if pass then Node.give_resource_start (get_index 0 node_index nodes) else ();
                  play_game Setup Setup board nodes turn pass true (add_node list node_index) ((turn, node_index, -1)::node) message card_list;
                end
            end
          else
            begin
              print_endline("Yellow Player, please select an edge next to your settlement to place a road");
              let selected_edge = select_edge () in
              if (not (if_edge turn selected_edge node)) then failwith "wrong position" else
                begin
                  Gamegraphics.draw_board board (build_road turn nodes selected_edge 0 []);
                  if (pass)then play_game Setup Setup board nodes (turn-1) pass false list ((turn, fst selected_edge, snd selected_edge)::node) message card_list
                  else play_game Setup Setup board nodes (turn+1) pass false list ((turn, fst selected_edge, snd selected_edge)::node) message card_list
                end
            end
        |3->
          if(not rd_ph) then
            begin
              print_endline("Blue player, please select a node to build a settlement.");
              let node_index =  select_node()  in
              if (if_neighbor 0 3 node_index list node) then failwith "wrong position" else
                begin
                  give_port node_index nodes turn;
                  Gamegraphics.draw_board board (build_settlement turn nodes node_index 0 [] "settlement");
                  if pass then Node.give_resource_start (get_index 0 node_index nodes) else ();
                  play_game Setup Setup board nodes turn pass true (add_node list node_index) ((turn, node_index, -1)::node) message card_list;
                end
            end
          else
            begin
              print_endline("Blue Player, please select an edge next to your settlement to place a road");
              let selected_edge = select_edge () in
              if (not (if_edge turn selected_edge node)) then failwith "wrong position" else
                begin
                  Gamegraphics.draw_board board (build_road turn nodes selected_edge 0 []);
                  if(pass)then play_game Setup Setup board nodes (turn-1) pass false list ((turn, fst selected_edge, snd selected_edge)::node) message card_list
                  else play_game Setup Setup board nodes turn true false list ((turn, fst selected_edge, snd selected_edge)::node) message card_list
                end
            end
        |_ -> raise(Failure("not a player"));)
     with
     |_-> (Gamegraphics.draw_board board nodes);
       play_game Setup Setup board nodes turn pass rd_ph list node message card_list;)

  |Help-> (
      (Gamegraphics.draw_board board nodes);
      print_endline("\nHelp Menu: The valid commands include");
      print_endline("done - when you are done with your turn");
      print_endline("inventory - access your inventory");
      print_endline("build city - build a city when it is your turn, and you have required resources");
      print_endline("build settlement - build a settlement when it is your turn, and you have required resources");
      print_endline("build road - build a road when it is your turn, and you have required resources");
      print_endline("help- display help menu");
      print_endline("quit- quit the game WARNING: progress will not be saved");
      print_endline("buycard- buy a development card");
      print_endline("use + cardname- use a development card you own");
      print_endline("cards- display your development cards");
      let input= Command.parse (read_line()) in
      ( match Command.to_data input with
        |("help",_,_,_,_)->play_game Help Help board nodes turn pass rd_ph list node message card_list
        |("done",_,_,_,_)->play_game prev_phase Help board nodes turn pass rd_ph list node message card_list
        |("quit",_,_,_,_)->play_game Quit Welcome board nodes turn pass rd_ph list node message card_list
        |_-> let msg = "Malformed command please re-enter" in
          play_game Help prev_phase board nodes turn pass rd_ph list node msg card_list;)
    );
  |Roll->
    let die_roll = random_roll () in
    distribute_resources nodes die_roll;
    (match die_roll with
     |7 -> let mes = "Players with more than 7 resources have had their resources cut in half" in
       play_game Robbing Roll board nodes turn pass rd_ph list node mes card_list
     |_ -> let mes = "The die roll resulted in a " ^ (string_of_int die_roll) ^
                     " and all of the resources have been distributed" in
       play_game Interactive Roll board nodes turn pass rd_ph list node mes card_list)
  |Interactive->
    if (prev_phase=Roll || prev_phase=AddCity || prev_phase=AddRoad || 
        prev_phase=AddSettle || prev_phase=BuyCard || prev_phase=UseKnight
        || prev_phase=UseProgress || prev_phase=UseVictory || prev_phase=Interactive) then
      (Gamegraphics.draw_board board nodes;
       let color = Player.player_to_string (get_index 0 turn player_list) in
       (match color with
        |"Magenta" -> ANSITerminal.(print_string [magenta] (message^"\n"));
        |"Yellow" -> ANSITerminal.(print_string [yellow] (message^"\n"));
        |"Blue" -> ANSITerminal.(print_string [cyan] (message^"\n"));
        |"Green" -> ANSITerminal.(print_string [green] (message^"\n"));
        | _ -> raise(Failure("Not a player color"))
       )
      )
    else if (prev_phase != Points && prev_phase != Inventory && prev_phase != Cards) then
      (Gamegraphics.draw_board board nodes;)
    else ();
    print_endline(
      "It the " ^ Player.player_to_string (get_index 0 turn player_list)
      ^ " players turn.");
    print_endline("Enter any command during your turn phase
    or help to view commands");
    let input= Command.parse (read_line()) in
    ( match Command.to_data input with
      |("help",_,_,_,_)->
        play_game Help Interactive board nodes turn pass rd_ph list node message card_list
      |("done",_,_,_,_)->
        if turn!=3 then
          play_game Roll Interactive board nodes (turn+1) pass rd_ph list node message card_list
        else
          play_game Roll Interactive board nodes 0 pass rd_ph list node message card_list
      |("quit",_,_,_,_)->
        play_game Quit Interactive board nodes turn pass rd_ph list node message card_list
      |("points",_,_,_,_)->
        play_game Points Interactive board nodes turn pass rd_ph list node message card_list
      |("inventory",_,_,_,_)->
        play_game Inventory Interactive board nodes turn pass rd_ph list node message card_list
      |("addcity",_,_,_,_)->
        play_game AddCity Interactive board nodes turn pass rd_ph list node message card_list
      |("addsettle",_,_,_,_)->
        play_game AddSettle Interactive board nodes turn pass rd_ph list node message card_list
      |("addroad",_,_,_,_)->
        play_game AddRoad Interactive board nodes turn pass rd_ph list node message card_list
      |("buycard",_,_,_,_)->
        play_game BuyCard Interactive board nodes turn pass rd_ph list node message card_list
      |("cards",_,_,_,_)->
        play_game Cards Interactive board nodes turn pass rd_ph list node message card_list
      |("useknight",_,_,_,_)->
        play_game UseKnight Interactive board nodes turn pass rd_ph list node message card_list
      |("useprogress",_,_,_,_)->
        play_game UseProgress Interactive board nodes turn pass rd_ph list node message card_list
      |("usevictory",_,_,_,_)->
        play_game UseVictory Interactive board nodes turn pass rd_ph list node message card_list
      |("tradebank",x,res1,y,res2)->
        let msg = "Invalid trade" in
        (match x,y with
         |(4,1)->
           if Player.has_trade_res (get_index 0 turn player_list) x res1 then
             (Player.bank_trade (get_index 0 turn player_list) 4 res1 1 res2;
              play_game Interactive Interactive board nodes turn pass rd_ph list node
                "Your trade has been completed" card_list)
           else play_game Interactive Interactive board nodes turn pass rd_ph list node msg card_list
         |(3,1)-> if Player.has_three_to_one (get_index 0 turn player_list) &&
                     (Player.has_trade_res (get_index 0 turn player_list) x res1) then
             (Player.bank_trade (get_index 0 turn player_list) 3 res1 1 res2;
              play_game Interactive Interactive board nodes turn pass rd_ph list node
                "Your trade has been completed" card_list)
           else play_game Interactive Interactive board nodes turn pass rd_ph list node msg card_list
         |(2,1)->if Player.has_two_to_one (get_index 0 turn player_list) res1 &&
                    (Player.has_trade_res (get_index 0 turn player_list) x res1) then
             (Player.bank_trade (get_index 0 turn player_list) 3 res1 1 res2;
              play_game Interactive Interactive board nodes turn pass rd_ph list node
                "Your trade has been completed" card_list)
           else play_game Interactive Interactive board nodes turn pass rd_ph list node msg card_list
         |(_,_)->
           play_game Interactive Interactive board nodes turn pass rd_ph list node msg card_list)
      |("tradeblue",x,res1,y,res2)->
        failwith ""
      |("tradegreen",x,res1,y,res2)->
        failwith ""
      |("trademagenta",x,res1,y,res2)->
        failwith ""
      |("tradeyellow",x,res1,y,res2)->
        failwith ""
      |_-> let msg = "Malformed command please re-enter" in
        play_game Interactive Interactive board nodes turn pass rd_ph list node msg card_list)
  |Robbing -> (
      try (Gamegraphics.draw_board board nodes;
           if prev_phase = UseKnight then print_endline("You use knight card, so Player " ^Player.player_to_string (get_index 0 turn player_list)^ " can now select the name (resource) of a tile to place the robber there")
           else
             print_endline("The die roll resulted in a 7, so Player " ^Player.player_to_string (get_index 0 turn player_list)^ " must now select the name (resource) of a tile to place the robber there");
           let rob_tile = select_tile () in
           let n_tile = robbers_false board;
             List.nth (List.rev board) (rob_tile - 1) in
           Tile.add_robber n_tile;
           play_game Interactive Roll board nodes turn pass rd_ph list node message card_list)
      with
      |_ -> play_game Robbing Robbing board nodes turn pass rd_ph list node message card_list
    )
  |AddSettle->(
      (*This try build the settlement ONLY if the player has enough resources*)
      if not(Player.can_build_set (get_index 0 turn player_list)) then
        (let msg = "You do not have enough resources to build a settlement" in
         play_game Interactive AddSettle board nodes turn pass rd_ph list node msg card_list;)
      else (
        try(
          Gamegraphics.draw_board board nodes;
          print_endline("Select a node to place a settlement");
          let node_index =  select_node() in
          if (if_neighbor 1 turn node_index list node) then failwith "wrong position" else
            begin
              (*you will have to build a settlement and pass in (get_index 0 node_index nodes) *)
              give_port node_index nodes turn;
              Player.build_settlement (get_index 0 turn player_list);
              Gamegraphics.draw_board board (build_settlement turn nodes node_index 0 [] "settlement");
              play_game Interactive AddSettle board nodes turn pass rd_ph (add_node list node_index) ((turn, node_index, -1)::node) "" card_list;
            end)
        with
        |_->play_game AddSettle AddSettle board nodes turn pass rd_ph list node message card_list));
  |AddCity->(
      (*This try build the settlement ONLY if the player has enough resources*)
      if not (Player.can_build_city (get_index 0 turn player_list)) then
        (let msg = "You do not have enough resources to build a city" in
         play_game Interactive AddCity board nodes turn pass rd_ph list node msg card_list;)
      else (
        try(
          Gamegraphics.draw_board board nodes;
          print_endline("Select a node to place a city");
          let node_index =  select_node() in
          if not (if_city turn node_index node) then failwith "wrong position" else
            begin
              Player.build_city (get_index 0 turn player_list);
              Gamegraphics.draw_board board (build_settlement turn nodes node_index 0 [] "city");
              play_game Interactive AddCity board nodes turn pass rd_ph (add_node list node_index) ((turn, node_index, -1)::node) "" card_list;
            end)
        with
        |_->play_game AddCity AddCity board nodes turn pass rd_ph list node message card_list));
  |AddRoad->(
      if not (Player.can_build_road (get_index 0 turn player_list)) then
        (let msg = "You do not have enough resources to build a road" in
         play_game Interactive AddRoad board nodes turn pass rd_ph list node msg card_list;)
      else (
        try(
          Gamegraphics.draw_board board nodes;
          print_endline("Select an edge to place a road");
          (*WE NEED TO CHECK IF YOU HAVE ENOUGH RESOURCES AND THEN TAKE THEM*)
          let selected_edge =  select_edge() in
          if (not (if_edge turn selected_edge node)) then failwith "wrong position" else
            begin
              Player.build_road (get_index 0 turn player_list);
              Gamegraphics.draw_board board (build_road turn nodes selected_edge 0 []);
              play_game Interactive AddRoad board nodes turn pass rd_ph list ((turn, fst selected_edge, snd selected_edge)::node) "" card_list;
            end)
        with
        |_->
          play_game AddRoad AddRoad board nodes turn pass rd_ph list node message card_list));
  |AddFreeRoad->(try(
      Gamegraphics.draw_board board nodes;
      print_endline("You use progress card, so you can select an edge to place a road for free");
      let selected_edge =  select_edge() in
      if (not (if_edge turn selected_edge node)) then failwith "wrong position" else
        begin
          Gamegraphics.draw_board board (build_road turn nodes selected_edge 0 []);
          play_game Interactive AddFreeRoad board nodes turn pass rd_ph list ((turn, fst selected_edge, snd selected_edge)::node) "" card_list;
        end)
     with
     |_->
       play_game AddFreeRoad AddFreeRoad board nodes turn pass rd_ph list node message card_list);
  |Inventory ->
    Gamegraphics.draw_board board nodes;
    print_endline("Your inventory includes: ");
    (match turn with
     |0 -> let list = (Player.resources_to_string (List.nth player_list 0))
       in List.iter (fun x -> print_string(x ^ ", ")) list
     |1 -> let list = (Player.resources_to_string (List.nth player_list 1))
       in List.iter (fun x -> print_string(x ^ ", ")) list
     |2 -> let list = (Player.resources_to_string (List.nth player_list 2))
       in List.iter (fun x -> print_string(x ^ ", ")) list
     |3 -> let list = (Player.resources_to_string (List.nth player_list 3))
       in List.iter (fun x -> print_string(x ^ ", ")) list
     |_ -> failwith("not a true number"));
    print_endline("");
    play_game prev_phase Inventory board nodes turn pass rd_ph list node message card_list;
  |Cards ->
    Gamegraphics.draw_board board nodes;
    print_endline("Your development cards include: ");
    (match turn with
     |0 -> let list = (Player.cards_to_string (List.nth player_list 0))
       in List.iter (fun x -> print_string(x ^ ", ")) list
     |1 -> let list = (Player.cards_to_string (List.nth player_list 1))
       in List.iter (fun x -> print_string(x ^ ", ")) list
     |2 -> let list = (Player.cards_to_string (List.nth player_list 2))
       in List.iter (fun x -> print_string(x ^ ", ")) list
     |3 -> let list = (Player.cards_to_string (List.nth player_list 3))
       in List.iter (fun x -> print_string(x ^ ", ")) list
     |_ -> failwith("not a true number"));
    print_endline("");
    play_game prev_phase Cards board nodes turn pass rd_ph list node message card_list;
  |Points->
    Gamegraphics.draw_board board nodes;
    (match turn with
     |0 -> print_int(Player.get_points(List.nth player_list 0))
     |1 -> print_int(Player.get_points(List.nth player_list 0))
     |2 -> print_int(Player.get_points(List.nth player_list 0))
     |3 -> print_int(Player.get_points(List.nth player_list 0))
     |_ -> failwith("not a true number"));
    print_endline("");
    play_game prev_phase Inventory board nodes turn pass rd_ph list node message card_list;
  |BuyCard->(
      (*This try buy development cards ONLY if the player has enough resources*)
      if not (Player.avail_card card_list) then 
        (let msg = "There is no available card" in
         play_game Interactive BuyCard board nodes turn pass rd_ph list node msg card_list;)
      else if not(Player.can_buy_card (get_index 0 turn player_list)) then
        (let msg = "You do not have enough resources to buy a card" in
         play_game Interactive BuyCard board nodes turn pass rd_ph list node msg card_list;)
      else (
        Gamegraphics.draw_board board nodes;
        let ran_card = random_card card_list in
        Player.buy_card (get_index 0 turn player_list) ran_card;
        play_game Interactive BuyCard board nodes turn pass rd_ph list node "You got a development card" (delete_card ran_card card_list);));
  |UseKnight-> (
      if not (Player.can_use_knight (get_index 0 turn player_list)) then
        (let msg = "You do not have knight card" in
         play_game Interactive UseKnight board nodes turn pass rd_ph list node msg card_list;)
      else ( Player.take_knight (get_index 0 turn player_list);
             play_game Robbing UseKnight board nodes turn pass rd_ph list node "" card_list;))
  |UseProgress->(     
      if not (Player.can_use_progress (get_index 0 turn player_list)) then
        (let msg = "You do not have progress card" in
         play_game Interactive UseProgress board nodes turn pass rd_ph list node msg card_list;)
      else ( Player.take_progress (get_index 0 turn player_list);
             play_game AddFreeRoad UseProgress board nodes turn pass rd_ph list node "" card_list;))
  |UseVictory->  (     
      if not (Player.can_use_victory (get_index 0 turn player_list)) then
        (let msg = "You do not have victory card" in
         play_game Interactive UseVictory board nodes turn pass rd_ph list node msg card_list;)
      else ( Player.take_victory (get_index 0 turn player_list);
             play_game Interactive UseVictory board nodes turn pass rd_ph list node "" card_list;))
  |Win->()
  |Quit->print_endline("\nThank you for playing!!!"); exit 0

let main () =
  let rand_board1 = rand_board () in
  play_game Welcome Welcome (rand_board1) (generate_nodes rand_board1) 0 false false [] [] "" Player.ini_card

(* Execute the game engine. *)
let () = main ()
