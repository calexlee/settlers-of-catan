type command = 
  | Quit
  | Inventory
  | AddCity
  | AddSettle
  | Done
  | Help

exception Empty

exception Malformed

(** [nonblank a] is helper function that checks if a string is blank *)
let nonblank string = 
  string <> ""

let parse str =
  match str |> String.split_on_char ' ' |> List.filter nonblank with
  | [] -> raise Empty
  | h::t -> if h = "quit" || h = "Quit" then begin
      if t = [] then Quit
      else raise Malformed
    end
    else if h = "inventory" || h = "Inventory" then begin
      if t = [] then Inventory
      else raise Malformed
    end
    else if h = "add" || h = "Add" then begin
      match t with
      | [] -> raise Malformed
      | h::t -> if h = "city"&& t=[] then AddCity
        else if h = "settlement"&&t=[] then AddSettle else raise Malformed
    end
    else if h="done" || h = "Done" then begin 
      if t = [] then Quit 
      else raise Malformed
    end
    else if h="help" || h = "Help" then begin 
      if t = [] then Help 
      else raise Malformed
    end
    else raise Malformed