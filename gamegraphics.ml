(* Graphics to be implemented here *)

(**[even_string_resource r] is the resource [r] with added spacing for display *)
let even_string_resource = function 
  | "wood" -> "wood  "
  | "brick"-> "brick "
  |"sheep" -> "sheep "
  |"wheat" -> "wheat "
  |"rock"  -> "rock  "
  |"desert"-> "desert"
  | _ -> raise(Failure("invalid resource"))

(**[even_string_number n] is the number [n] with added spacing for display *)
let even_string_number = function
  |0 -> "0 "
  |2 -> "2 "
  |3 -> "3 "
  |4 -> "4 "
  |5 -> "5 "
  |6 -> "6 "
  |7 -> "7 "
  |8 -> "8 "
  |9 -> "9 "
  |10 -> "10"
  |11 -> "11"
  |12 -> "12"
  | _ -> raise(Failure("invalid number"))

(**[find_node_with_idx idx nlist] is the node with index [idx] in [nlist] *)
let rec find_node_with_idx idx (nlist : Node.t list) = 
  match nlist with  
  |[] -> raise(Not_found)
  |h::t -> if Node.get_index h = idx then h else find_node_with_idx idx t

(**[is_player_node n] is the letter of the player color, repeated twice if it
   if there is a city at node [n], otherwise [lg] if player is None *)
let is_player_node n lg= 
  try
    match Node.get_player n with 
    |p -> if String.equal (Node.get_settlement n) "settlement" 
      then Player.color_to_string (Player.get_color p)
      else 
        String.concat "" 
          [Player.color_to_string (Player.get_color p);
           Player.color_to_string (Player.get_color p)]
  with 
  |Not_found -> lg


(**[settlement_draw s i] is the string of the settlement at position [i] 
   in the list of nodes [s] *)
let settlement_draw s i =
  match (i+1) with
  |1|3|5|7|9|11|14|16|18|19|21|23|26|28|30|31|33|35|38|40|42|43|45|47|49|51|53
    -> (try let n = find_node_with_idx i s in 
          is_player_node n ">" 
        with 
        |Not_found -> ">")
  |2|4|6|8|10|12|13|15|17|20|22|24|25|27|29|32|34|36|37|39|41|44|46|48|50|52|54
    -> (try let n = find_node_with_idx i s in 
          is_player_node n "<"
        with 
        |Not_found -> "<")
  | _ -> raise(Failure("invalid index"))

(**[grab_resource t n] is the resource of the tile at position [n] in [t] *)
let grab_resource t n =     
  Board.grab_resource t n |> even_string_resource
(**[grab_resource t n] is the number of the tile at position [n] in [t] *)
let grab_num t n = 
  Board.grab_num t n |> even_string_number

let rc_to_node rc = 
  match rc with 
  |	(36,40)	-> 53
  |	(36,34)	-> 52
  |	(33,49)	-> 51
  |	(33,43)	-> 50
  |	(33,31)	-> 49
  |	(33,25)	-> 48
  |	(30,58)	-> 47
  |	(30,52)	-> 46
  |	(30,40)	-> 45
  |	(30,34)	-> 44
  |	(30,22)	-> 43
  |	(30,16)	-> 42
  |	(27,61)	-> 41
  |	(27,49)	-> 40
  |	(27,43)	-> 39
  |	(27,31)	-> 38
  |	(27,25)	-> 37
  |	(27,13)	-> 36
  |	(24,58)	-> 35
  |	(24,52)	-> 34
  |	(24,40)	-> 33
  |	(24,34)	-> 32
  |	(24,22)	-> 31
  |	(24,16)	-> 30
  |	(21,61)	-> 29
  |	(21,49)	-> 28
  |	(21,43)	-> 27
  |	(21,31)	-> 26
  |	(21,25)	-> 25
  |	(21,13)	-> 24
  |	(18,58)	-> 23
  |	(18,52)	-> 22
  |	(18,40)	-> 21
  |	(18,34)	-> 20
  |	(18,22)	-> 19
  |	(18,16)	-> 18
  |	(15,62)	-> 17
  |	(15,49)	-> 16
  |	(15,43)	-> 15
  |	(15,31)	-> 14
  |	(15,25)	-> 13
  |	(15,13)	-> 12
  |	(12,57)	-> 11
  |	(12,51)	-> 10
  |	(12,40)	-> 9
  |	(12,34)	-> 8
  |	(12,22)	-> 7
  |	(12,16)	-> 6
  |	(9,49)	-> 5
  |	(9,43)	-> 4
  |	(9,32)	-> 3
  |	(9,26)	-> 2
  |	(6,40)	-> 1
  |	(6,34)	-> 0
  |_ -> raise(Failure"Not a Node")

let rc_to_edge = function
  |(6,39)|(6,38)|(6,37)|(6,36)|(6,35) -> (0,1)
  |(9,31)|(9,30)|(9,29)|(9,28)|(9,27) -> (2,3)
  |(9,48)|(9,47)|(9,46)|(9,45)|(9,44) -> (4,5)
  |(12,21)|(12,20)|(12,19)|(12,18)|(12,17) -> (6,7)
  |(12,39)|(12,38)|(12,37)|(12,36)|(12,35) -> (8,9)
  |(12,56)|(12,55)|(12,54)|(12,53)|(12,52) -> (10,11)
  |(15,26)|(15,27)|(15,28)|(15,29)|(15,30) -> (13,14)
  |(15,44)|(15,45)|(15,46)|(15,47)|(15,48) -> (15,16)
  | (18,17)| (18,18)| (18,19)| (18,20)| (18,21)	->	(18,19)
  | (18,35)| (18,36)| (18,37)| (18,38)| (18,39)	->	(20,21)
  | (18,53)| (18,54)| (18,55)| (18,56)| (18,57)	->	(22,23)
  | (21,26)| (21,27)| (21,28)| (21,29)| (21,30)	->	(25,26)
  | (21,44)| (21,45)| (21,46)| (21,47)| (21,48)	->	(27,28)
  | (24,17)| (24,18)| (24,19)| (24,20)| (24,21)	->	(30,31)
  | (24,35)| (24,36)| (24,37)| (24,38)| (24,39)	->	(32,33)
  | (24,53)| (24,54)| (24,55)| (24,56)| (24,57)	->	(34,35)
  | (27,26)| (27,27)| (27,28)| (27,29)| (27,30)	->	(37,38)
  | (27,44)| (27,45)| (27,46)| (27,47)| (27,48)	->	(39,40)
  | (30,17)| (30,18)| (30,19)| (30,20)| (30,21)	->	(42,43)
  | (30,35)| (30,36)| (30,37)| (30,38)| (30,39)	->	(44,45)
  | (30,53)| (30,54)| (30,55)| (30,56)| (30,57)	->	(46,47)
  | (33,26)| (33,27)| (33,28)| (33,29)| (33,30)	->	(48,49)
  | (33,44)| (33,45)| (33,46)| (33,47)| (33,48)	->	(50,51)
  | (36,35)| (36,36)| (36,37)| (36,38)| (36,39)	->	(52,53)
  |(8,32)|(7,33) -> (0,3)
  |(8,42)|(7,41) -> (1,4)
  |(11,23)|(10,24) -> (2,7)
  |(11,33)|(10,32) -> (3,8)
  |(11,41)|(10,42) -> (4,9)
  |(11,51)|(10,50) -> (5,10)
  |(14,14)|(13,15) -> (6,12)
  |(14,24)|(13,23) ->(7,13)
  |(14,32)|(13,33) -> (8,14)
  |(14,42)|(13,41) -> (9,15)
  |(14,50)|(13,51) ->(10,16)
  |(14,60)|(13,59) -> (11,17)
  |(17,15)|(16,14) -> (12,18)
  |(17,23)|(16,24) -> (13,19)
  |(17,33)|(16,32) -> (14,20)
  |(17,41)|(16,42) -> (15,21)
  |(17,51)|(16,50) -> (16,22)
  |(17,59)|(16,60) -> (17,23)
  |(19,15)|(20,14)	->	(18,24)
  |(19,23)|(20,24)	->	(19,25)
  |(19,33)|(20,32)	->	(20,26)
  |(19,41)|(20,42)	->	(21,27)
  |(19,51)|(20,50)	->	(22,28)
  |(19,59)|(20,60)	->	(23,29)
  |(22,14)|(23,15)	->	(24,30)
  |(22,24)|(23,23)	->	(25,31)
  |(22,32)|(23,33)	->	(26,32)
  |(22,42)|(23,41)	->	(27,33)
  |(22,50)|(23,51)	->	(28,34)
  |(22,60)|(23,59)	->	(29,35)
  |(25,15)|(26,14)	->	(30,36)
  |(25,23)|(26,24)	->	(31,37)
  |(25,33)|(26,32)	->	(32,38)
  |(25,41)|(26,42)	->	(33,39)
  |(25,51)|(26,50)	->	(34,40)
  |(25,59)|(26,60)	->	(35,41)
  |(28,14)|(29,15)	->	(36,42)
  |(28,24)|(29,23)	->	(37,43)
  |(28,32)|(29,33)	->	(38,44)
  |(28,42)|(29,41)	->	(39,45)
  |(28,50)|(29,51)	->	(40,46)
  |(28,60)|(29,59)	->	(41,47)
  |(31,23)|(32,24)	->	(43,48)
  |(31,33)|(32,32)	->	(44,49)
  |(31,41)|(32,42)	->	(45,50)
  |(31,51)|(32,50)	->	(46,51)
  |(34,32)|(35,33)	->	(49,52)
  |(34,42)|(35,41)	->	(50,53)
  | _ -> raise(Failure "Not an Edge")

let draw_board t n = 
  ANSITerminal.resize 1080 720;
  ignore(Sys.command "clear");
  print_string("                                  >-----< \n");
  print_string("                                 /~~~~~~~\\ \n");
  print_string("                                /~~~~~~~~~\\ \n");
  print_string("                         >-----<~~~~3:1~~~~>-----< \n");
  print_string("                        /~~~~~~~\\~~~~~~~~~/~~~~~~~\\ \n");
  print_string("                       /~~~~~~~~~\\*~~~~~*/~~~~~~~~~\\ \n");
  print_string("                >-----<~~~~~~~~~~~");print_string(settlement_draw n 0);print_string("-----");print_string(settlement_draw n 1);print_string("~~~~~~~~~~~>-----< \n");
  print_string("               /~~~~~~~\\~~~~~~~~~/       \\~~~~~~~~~/~~~~~~~\\ \n");
  print_string("              /~~~2:1~~~\\~~~~~~~/   ");print_string(grab_num t 0);print_string("    \\~~~~~~~/~~~2:1~~~\\ \n");
  print_string("       >-----<~~~sheep~~~*");print_string(settlement_draw n 2);print_string("-----");print_string(settlement_draw n 3);print_string("  ");print_string(grab_resource t 0);print_string("  ");print_string(settlement_draw n 4);print_string("-----");print_string(settlement_draw n 5);print_string("*~~rock~~~>-----< \n");
  print_string("      /~~~~~~~\\~~~~~~~~~/       \\         /       \\~~~~~~~~~/~~~~~~~\\ \n");
  print_string("     /~~~~~~~~~\\~~~~~~*/   ");print_string(grab_num t 1);print_string("    \\       /    ");print_string(grab_num t 2);print_string("   \\*~~~~~~/~~~~~~~~~\\ \n");
  print_string("    <~~~~~~~~~~~");print_string(settlement_draw n 6);print_string("-----");print_string(settlement_draw n 7);print_string("   ");print_string(grab_resource t 1);print_string("  ");print_string(settlement_draw n 8);print_string("-----");print_string(settlement_draw n 9);print_string("  ");print_string(grab_resource t 2);print_string("  ");print_string(settlement_draw n 10);print_string("-----");print_string(settlement_draw n 11);print_string("~~~~~~~~~~~> \n");
  print_string("     \\~~~~~~~~~/       \\         /       \\         /       \\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~~/    ");print_string(grab_num t 3);print_string("   \\       /    ");print_string(grab_num t 4);print_string("   \\       /    ");print_string(grab_num t 5);print_string("   \\~~~~~~~/ \n");
  print_string("       >-----");print_string(settlement_draw n 12);print_string("  ");print_string(grab_resource t 3);print_string("   ");print_string(settlement_draw n 13);print_string("-----");print_string(settlement_draw n 14);print_string("   ");print_string(grab_resource t 4);print_string("  ");print_string(settlement_draw n 15);print_string("-----");print_string(settlement_draw n 16);print_string("   ");print_string(grab_resource t 5);print_string("   ");print_string(settlement_draw n 17);print_string("-----< \n");
  print_string("      /~~~~~~~\\         /       \\         /       \\         /~~~~~~~\\ \n");
  print_string("     /~~~2:1~~~\\       /    ");print_string(grab_num t 6);print_string("   \\       /         \\       /~~~2:1~~~\\ \n");
  print_string("    <~~~wood~~* ");print_string(settlement_draw n 18);print_string("-----");print_string(settlement_draw n 19);print_string("  ");print_string(grab_resource t 6);print_string("   ");print_string(settlement_draw n 20);print_string("-----");print_string(settlement_draw n 21);print_string("     ");print_string(grab_num t 7);print_string("    ");print_string(settlement_draw n 22);print_string("-----");print_string(settlement_draw n 23);print_string("*~~ore~~~~> \n");
  print_string("     \\~~~~~~~~~/       \\         /       \\  ");print_string(grab_resource t 7);print_string(" /       \\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~*/    ");print_string(grab_num t 8);print_string("   \\       /    ");print_string(grab_num t 9);print_string("   \\       /    ");print_string(grab_num t 10);print_string("   \\*~~~~~~/ \n");
  print_string("       >-----");print_string(settlement_draw n 24);print_string("  ");print_string(grab_resource t 8);print_string("   ");print_string(settlement_draw n 25);print_string("-----");print_string(settlement_draw n 26);print_string("   ");print_string(grab_resource t 9);print_string("  ");print_string(settlement_draw n 27);print_string("-----");print_string(settlement_draw n 28);print_string("   ");print_string(grab_resource t 10);print_string("  ");print_string(settlement_draw n 29);print_string("-----< \n");
  print_string("      /~~~~~~~\\         /       \\         /       \\         /~~~~~~~\\ \n");
  print_string("     /~~~~~~~~~\\       /    ");print_string(grab_num t 11);print_string("   \\       /    ");print_string(grab_num t 12);print_string("   \\       /~~~~~~~~~\\ \n");
  print_string("    <~~~~~~~~~~~");print_string(settlement_draw n 30);print_string("-----");print_string(settlement_draw n 31);print_string("  ");print_string(grab_resource t 11);print_string("   ");print_string(settlement_draw n 32);print_string("-----");print_string(settlement_draw n 33);print_string("   ");print_string(grab_resource t 12);print_string("  ");print_string(settlement_draw n 34);print_string("-----");print_string(settlement_draw n 35);print_string("~~~~~~~~~~~>  \n");
  print_string("     \\~~~~~~~~~/       \\         /       \\         /       \\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~~/    ");print_string(grab_num t 13);print_string("   \\       /    ");print_string(grab_num t 14);print_string("   \\       /    ");print_string(grab_num t 15);print_string("   \\~~~~~~~/ \n");
  print_string("       >-----");print_string(settlement_draw n 36);print_string("   ");print_string(grab_resource t 13);print_string("  ");print_string(settlement_draw n 37);print_string("-----");print_string(settlement_draw n 38);print_string("   ");print_string(grab_resource t 14);print_string("  ");print_string(settlement_draw n 39);print_string("-----");print_string(settlement_draw n 40);print_string("   ");print_string(grab_resource t 15);print_string("  ");print_string(settlement_draw n 41);print_string("-----< \n");
  print_string("      /~~~~~~*\\         /       \\         /       \\         /*~~~~~~\\ \n");
  print_string("     /~~~~~~~~~\\       /    ");print_string(grab_num t 16);print_string("   \\       /    ");print_string(grab_num t 17);print_string("   \\       /~~~~~~~~~\\ \n");
  print_string("    <~~~~3:1~~~*");print_string(settlement_draw n 42);print_string("-----");print_string(settlement_draw n 43);print_string("   ");print_string(grab_resource t 16);print_string("  ");print_string(settlement_draw n 44);print_string("-----");print_string(settlement_draw n 45);print_string("   ");print_string(grab_resource t 17);print_string("  ");print_string(settlement_draw n 46);print_string("-----");print_string(settlement_draw n 47);print_string("*~~~3:1~~~~> \n");
  print_string("     \\~~~~~~~~~/~~~~~~~\\         /       \\         /~~~~~~~\\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~~/~~~~~~~~~\\       /    ");print_string(grab_num t 18);print_string("   \\       /~~~~~~~~~\\~~~~~~~/ \n");
  print_string("       >-----< ~~~~~~~~~~");print_string(settlement_draw n 48);print_string("-----");print_string(settlement_draw n 49);print_string("   ");print_string(grab_resource t 18);print_string("  ");print_string(settlement_draw n 50);print_string("-----");print_string(settlement_draw n 51);print_string("~~~~~~~~~~~>-----< \n");
  print_string("              \\~~~~~~~~~/*~~~~~*\\         /*~~~~~*\\~~~~~~~~~/ \n");
  print_string("               \\~~~~~~~/~~~~~~~~~\\       /~~~2:1~~~\\~~~~~~~/ \n");
  print_string("                >-----<~~~~3:1~~~~");print_string(settlement_draw n 52);print_string("-----");print_string(settlement_draw n 53);print_string("~~~wheat~~~>-----< \n");
  print_string("                       \\~~~~~~~~~/~~~~~~~\\~~~~~~~~~/ \n");
  print_string("                        \\~~~~~~~/~~~~~~~~~\\~~~~~~~/ \n");
  print_string("                         >-----<~~~~~~~~~~~>-----< \n");
  print_string("                                \\~~~~~~~~~/ \n");
  print_string("                                 \\~~~~~~~/ \n");
  print_string("                                  >-----< \n");
