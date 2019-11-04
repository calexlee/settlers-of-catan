(**The type of some player *)
type node_player

(**The type of Road *)
type rd

(** The abstract type of values representing adventures. *)
type t

(**[make_edge n] returns a new edge t with index n.*)
val make_edge : int -> t

(**[add_road t p] adds a road belonging to p to edge t.*)
val add_road : t -> node_player -> unit

(**[remove_road t] removes the road belonging t.*)
val remove_road : t -> unit

(**[get_index t] returns the index of [t] *)
val get_index : t -> int

