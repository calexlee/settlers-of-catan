
(**The type of a settlement *)
type s
(**The type of some player *)
type node_player

(**The type of a Node *)
type t

(**[make_node list n edge] returns a new node t.*)
val make_node : Tile.t list -> int -> Edge.t list -> t

(** [add_settlement a t] can add a new settlement a to t.*)
val add_settlement : string -> t -> unit

(** [add_player p] can add a new player p to t.*)
val add_player : node_player -> t -> unit

(**[give_resource dr node] give the player who has a settlement on node [node] 
   all resources corresponding to die roll [dr]*)
val give_resource : int -> t -> unit
