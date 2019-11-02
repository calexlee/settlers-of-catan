(* Graphics to be implemented here *)

let grab_resource t n =     
  List.nth t n |> Tile.get_resource

let draw_board t = 
  print_string("                                  >-----< \n");
  print_string("                                 /~~~~~~~\\ \n");
  print_string("                                /~~~~~~~~~\\ \n");
  print_string("                         >-----<~~~~3:1~~~~>-----< \n");
  print_string("                        /~~~~~~~\\~~~~~~~~~/~~~~~~~\\ \n");
  print_string("                       /~~~~~~~~~\\*~~~~~*/~~~~~~~~~\\ \n");
  print_string("                >-----<~~~~~~~~~~~>-----<~~~~~~~~~~~>-----< \n");
  print_string("               /~~~~~~~\\~~~~~~~~~/       \\~~~~~~~~~/~~~~~~~\\ \n");
  print_string("              /~~~2:1~~~\\~~~~~~~/    #    \\~~~~~~~/~~~2:1~~~\\ \n");
  print_string("       >-----<~~~sheep~~~*>-----<   ");print_string(grab_resource t 0);print_string("    >-----<*~~rock~~~>-----< \n");
  print_string("      /~~~~~~~\\~~~~~~~~~/       \\         /       \\~~~~~~~~~/~~~~~~~\\ \n");
  print_string("     /~~~~~~~~~\\~~~~~~*/    #    \\       /    #    \\*~~~~~~/~~~~~~~~~\\ \n");
  print_string("    <~~~~~~~~~~~>-----<   ");print_string(grab_resource t 1);print_string("    >-----<  ");print_string(grab_resource t 2);print_string("   >-----<~~~~~~~~~~~> \n");
  print_string("     \\~~~~~~~~~/       \\         /       \\         /       \\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~~/    #    \\       /    #    \\       /    #    \\~~~~~~~/ \n");
  print_string("       >-----<  ");print_string(grab_resource t 3);print_string("   >-----<   ");print_string(grab_resource t 4);print_string("    >-----<   ");print_string(grab_resource t 5);print_string("    >-----< \n");
  print_string("      /~~~~~~~\\         /       \\         /       \\         /~~~~~~~\\ \n");
  print_string("     /~~~2:1~~~\\       /    #    \\       /         \\       /~~~2:1~~~\\ \n");
  print_string("    <~~~wood~~* >-----<  ");print_string(grab_resource t 6);print_string("     >-----<     #    >-----<*~~ore~~~~> \n");
  print_string("     \\~~~~~~~~~/       \\         /       \\  ");print_string(grab_resource t 7);print_string(" /       \\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~*/    #    \\       /    #    \\       /    #    \\*~~~~~~/ \n");
  print_string("       >-----<  ");print_string(grab_resource t 8);print_string("    >-----<   ");print_string(grab_resource t 9);print_string("   >-----<   ");print_string(grab_resource t 10);print_string("    >-----< \n");
  print_string("      /~~~~~~~\\         /       \\         /       \\         /~~~~~~~\\ \n");
  print_string("     /~~~~~~~~~\\       /    #    \\       /    #    \\       /~~~~~~~~~\\ \n");
  print_string("    <~~~~~~~~~~~>-----<  ");print_string(grab_resource t 11);print_string("   >-----<   ");print_string(grab_resource t 12);print_string("    >-----<~~~~~~~~~~~>  \n");
  print_string("     \\~~~~~~~~~/       \\         /       \\         /       \\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~~/    #    \\       /    #    \\       /    #    \\~~~~~~~/ \n");
  print_string("       >-----<   ");print_string(grab_resource t 13);print_string("   >-----<   ");print_string(grab_resource t 14);print_string("    >-----<   ");print_string(grab_resource t 15);print_string("   >-----< \n");
  print_string("      /~~~~~~*\\         /       \\         /       \\         /*~~~~~~\\ \n");
  print_string("     /~~~~~~~~~\\       /    #    \\       /    #    \\       /~~~~~~~~~\\ \n");
  print_string("    <~~~~3:1~~~*>-----<   ");print_string(grab_resource t 16);print_string("   >-----<   ");print_string(grab_resource t 17);print_string("    >-----<*~~~3:1~~~~> \n");
  print_string("     \\~~~~~~~~~/~~~~~~~\\         /       \\         /~~~~~~~\\~~~~~~~~~/ \n");
  print_string("      \\~~~~~~~/~~~~~~~~~\\       /    #    \\       /~~~~~~~~~\\~~~~~~~/ \n");
  print_string("       >-----< ~~~~~~~~~~>-----<   ");print_string(grab_resource t 18);print_string("   >-----<~~~~~~~~~~~>-----< \n");
  print_string("              \\~~~~~~~~~/*~~~~~*\\         /*~~~~~*\\~~~~~~~~~/ \n");
  print_string("               \\~~~~~~~/~~~~~~~~~\\       /~~~2:1~~~\\~~~~~~~/ \n");
  print_string("                >-----<~~~~3:1~~~~>-----<~~~wheat~~~>-----< \n");
  print_string("                       \\~~~~~~~~~/~~~~~~~\\~~~~~~~~~/ \n");
  print_string("                        \\~~~~~~~/~~~~~~~~~\\~~~~~~~/ \n");
  print_string("                         >-----<~~~~~~~~~~~>-----< \n");
  print_string("                                \\~~~~~~~~~/ \n");
  print_string("                                 \\~~~~~~~/ \n");
  print_string("                                  >-----< \n");

