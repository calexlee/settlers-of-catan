type command = 
  | Quit
  | Inventory
  | AddCity
  | AddSettle

exception Empty

exception Malformed

(** [nonblank a] is helper function that checks if a string is blank *)
let nonblank string = 
  string <> ""

let parse str =
  match str |> String.split_on_char ' ' |> List.filter nonblank with
  | [] -> raise Empty
  | h::t -> if h = "quit" then begin
      if t = [] then Quit
      else raise Malformed
    end
    else if h = "inventory" then begin
      if t = [] then Inventory
      else raise Malformed
    end
    else if h = "add" then begin
      match t with
      | [] -> raise Malformed
      | h::t -> if h = "city"&& t=[] then AddCity
        else if h = "settlement"&&t=[] then AddSettle else raise Malformed
    end
    else raise Malformed