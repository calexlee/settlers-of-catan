type command = 
  | Quit
  | Inventory
  | AddCity
  | AddSettle
  | AddRoad
  | Done
  | Help
  | Points
  | Malformed

exception Empty

exception Malformed

(** [nonblank a] is helper function that checks if a string is blank *)
let nonblank string = 
  string <> ""

let to_string command = 
  match command with 
  |Quit -> "quit"
  |Inventory -> "inventory"
  |AddCity -> "addcity"
  |AddSettle -> "addsettle"
  |AddRoad -> "addroad"
  |Done-> "done"
  |Help -> "help"
  |Points -> "points"
  |Malformed -> "malformed"

let parse str =
  try (
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
      else if h = "build" || h = "Build" then begin
        match t with
        | [] -> raise Malformed
        | h::t -> if (h = "city" || h="City")&& t=[] then AddCity
          else if (h = "settlement"|| h= "Settlement") && t=[] then AddSettle 
          else if (h= "road" || h = "Road") && t=[] then AddRoad
          else raise Malformed
      end
      else if h= "done" || h = "Done" then begin 
        if t = [] then Done
        else raise Malformed
      end
      else if h="help" || h = "Help" then begin 
        if t = [] then Help 
        else raise Malformed
      end
      else if h="points" || h = "Points" then begin 
        if t = [] then Points
        else raise Malformed
      end
      else raise Malformed)
  with 
  |Malformed -> Malformed