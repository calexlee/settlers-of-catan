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
    -> (try let n = find_node_with_idx (i+1) s in 
          is_player_node n ">" 
        with 
        |Not_found -> ">")
  |2|4|6|8|10|12|13|15|17|20|22|24|25|27|29|32|34|36|37|39|41|44|46|48|50|52|54
    -> (try let n = find_node_with_idx (i+1) s in 
          is_player_node n "<"
        with 
        |Not_found -> "<")
  | _ -> raise(Failure("invalid index"))



(**[grab_resource t n] is the resource of the tile at position [n] in [t] *)
let grab_resource t n =     
  List.nth t n |> Tile.get_resource |> even_string_resource
(**[grab_resource t n] is the number of the tile at position [n] in [t] *)
let grab_num t n = 
  List.nth t n |> Tile.get_number |> even_string_number

let draw_board t n = 
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

