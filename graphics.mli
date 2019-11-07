(*  A module that creates a graphical user interface by displaying all 
    the tiles on the board and drawing out all the nodes and edges with houses,
     cities or roads on them.
*)

(**[draw_board t] draws the board [t] in the terminal *) 
val draw_board : Board.t -> Node.t list -> unit