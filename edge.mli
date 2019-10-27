module type Edge = sig 

  (**The type of Road *)
  type rd = {
    is_there: bool;
    (*player: Player.player;*)
  }

  (** The abstract type of values representing adventures. *)
  type t = {
    road: rd;
    index: int;
  }


end