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
  |[] -> raise(Failure("No Node Found with index"))
  |h::t -> if Node.get_index h = idx then h else find_node_with_idx idx t

(**[settlement_draw s i] draws the settlement at position [i] 
   in the list of nodes [s] *)
let settlement_draw s i =
  match i with
  |1|3|5|7|9|11|14|16|18|19|21|23|26|28|30|31|33|35|38|40|42|43|45|47|49|51|53
    -> let n = find_node_with_idx i s in 
    failwith("unimplemented")
  |2|4|6|8|10|12|13|15|17|20|22|24|25|27|29|32|34|36|37|39|41|44|46|48|50|52|54
    -> let n = find_node_with_idx i s in 
    failwith("unimplemented")
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
  print_string("                >-----<~~~~~~~~~~~>-----<~~~~~~~~~~~>-----< \n");
  print_string("               /~~~~~~~\\~~~~~~~~~/       \\~~~~~~~~~/~~~~~~~\\ \n");
  print_string("              /~~~2:1~~~\\~~~~~~~/   ");print_string(grab_num t 0);print_string("    \\~~~~~~~/~~~2:1~~~\\ \n");
  print_string("       >-----<~~~sheep~~~*>-----<  ");print_string(grab_resource t 0);print_string("  >-----<*~~rock~~~>-----< \n");
  print_string("      /~~~~~~~\\~~~~~~~~~/       \\         /       \\~~~~~~~~~/~~~~~~~\\ \n");
  print_string("     /~~~~~~~~~\\~~~~~~*/   ");print_string(grab_num t 1);print_string("    \\       /    ");print_string(grab_num t 2);print_string("   \\*~~~~~~/~~~~~~~~~\\ \n");
  print_string("    <~~~~~~~~~~~>-----<   ");print_string(grab_resource t 1);print_string("  >-----<  ");print_string(grab_resource t 2);print_string("  >-----<~~~~~~~~~~~> \n");
  print_string("     \\~~~~~~~~~/       \\         /       \\         /       \\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~~/    ");print_string(grab_num t 3);print_string("   \\       /    ");print_string(grab_num t 4);print_string("   \\       /    ");print_string(grab_num t 5);print_string("   \\~~~~~~~/ \n");
  print_string("       >-----<  ");print_string(grab_resource t 3);print_string("   >-----<   ");print_string(grab_resource t 4);print_string("  >-----<   ");print_string(grab_resource t 5);print_string("   >-----< \n");
  print_string("      /~~~~~~~\\         /       \\         /       \\         /~~~~~~~\\ \n");
  print_string("     /~~~2:1~~~\\       /    ");print_string(grab_num t 6);print_string("   \\       /         \\       /~~~2:1~~~\\ \n");
  print_string("    <~~~wood~~* >-----<  ");print_string(grab_resource t 6);print_string("   >-----<     ");print_string(grab_num t 7);print_string("    >-----<*~~ore~~~~> \n");
  print_string("     \\~~~~~~~~~/       \\         /       \\  ");print_string(grab_resource t 7);print_string(" /       \\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~*/    ");print_string(grab_num t 8);print_string("   \\       /    ");print_string(grab_num t 9);print_string("   \\       /    ");print_string(grab_num t 10);print_string("   \\*~~~~~~/ \n");
  print_string("       >-----<  ");print_string(grab_resource t 8);print_string("   >-----<   ");print_string(grab_resource t 9);print_string("  >-----<   ");print_string(grab_resource t 10);print_string("  >-----< \n");
  print_string("      /~~~~~~~\\         /       \\         /       \\         /~~~~~~~\\ \n");
  print_string("     /~~~~~~~~~\\       /    ");print_string(grab_num t 11);print_string("   \\       /    ");print_string(grab_num t 12);print_string("   \\       /~~~~~~~~~\\ \n");
  print_string("    <~~~~~~~~~~~>-----<  ");print_string(grab_resource t 11);print_string("   >-----<   ");print_string(grab_resource t 12);print_string("  >-----<~~~~~~~~~~~>  \n");
  print_string("     \\~~~~~~~~~/       \\         /       \\         /       \\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~~/    ");print_string(grab_num t 13);print_string("   \\       /    ");print_string(grab_num t 14);print_string("   \\       /    ");print_string(grab_num t 15);print_string("   \\~~~~~~~/ \n");
  print_string("       >-----<   ");print_string(grab_resource t 13);print_string("  >-----<   ");print_string(grab_resource t 14);print_string("  >-----<   ");print_string(grab_resource t 15);print_string("  >-----< \n");
  print_string("      /~~~~~~*\\         /       \\         /       \\         /*~~~~~~\\ \n");
  print_string("     /~~~~~~~~~\\       /    ");print_string(grab_num t 16);print_string("   \\       /    ");print_string(grab_num t 17);print_string("   \\       /~~~~~~~~~\\ \n");
  print_string("    <~~~~3:1~~~*>-----<   ");print_string(grab_resource t 16);print_string("  >-----<   ");print_string(grab_resource t 17);print_string("  >-----<*~~~3:1~~~~> \n");
  print_string("     \\~~~~~~~~~/~~~~~~~\\         /       \\         /~~~~~~~\\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~~/~~~~~~~~~\\       /    ");print_string(grab_num t 18);print_string("   \\       /~~~~~~~~~\\~~~~~~~/ \n");
  print_string("       >-----< ~~~~~~~~~~>-----<   ");print_string(grab_resource t 18);print_string("  >-----<~~~~~~~~~~~>-----< \n");
  print_string("              \\~~~~~~~~~/*~~~~~*\\         /*~~~~~*\\~~~~~~~~~/ \n");
  print_string("               \\~~~~~~~/~~~~~~~~~\\       /~~~2:1~~~\\~~~~~~~/ \n");
  print_string("                >-----<~~~~3:1~~~~>-----<~~~wheat~~~>-----< \n");
  print_string("                       \\~~~~~~~~~/~~~~~~~\\~~~~~~~~~/ \n");
  print_string("                        \\~~~~~~~/~~~~~~~~~\\~~~~~~~/ \n");
  print_string("                         >-----<~~~~~~~~~~~>-----< \n");
  print_string("                                \\~~~~~~~~~/ \n");
  print_string("                                 \\~~~~~~~/ \n");
  print_string("                                  >-----< \n");

