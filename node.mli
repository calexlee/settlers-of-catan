
(**The type of a settlement *)
type s

(**The type of a Node *)
type t

(**[give_resource dr node] give the player who has a settlement on node [node] 
   all resources corresponding to die roll [dr]*)
val give_resource : int -> t -> unit

(**[get_index t] is the index of [t] *)
val get_index : t -> int