(* Player to be implemented here *)
type r = Wood | Brick | Wheat | Sheep | Rock | Desert
type color = Green | Magenta | Yellow | Blue
type t = {
  color : color;
  mutable resources: r list;
  mutable points: int;
  (*mutable card_list: *)
  mutable longest_road: bool;
}

let make_player color= 
  let colorp = 
    match color with
    |x when x="green" -> Green
    |x when x="magenta" -> Magenta
    |x when x="yellow" -> Yellow
    |x when x="blue" -> Blue 
    |_ -> Green in
  {
    color = colorp; (* Need to make random out of available options*)
    resources = [];
    points = 2;
    (*card_list = *)
    longest_road = false;
  }

let num_of_res t = 
  List.length t.resources

let get_points t =
  t.points

let get_resources t =
  t.resources

let get_color t =
  t.color

let color_to_string c = 
  match c with 
  |Green -> "G"
  |Magenta -> "M"
  |Yellow -> "Y"
  |Blue -> "B"

let give_sheep t =
  t.resources <- Sheep :: t.resources

let give_rock t =
  t.resources <- Rock :: t.resources

let give_brick t =
  t.resources <- Brick :: t.resources

let give_wood t =
  t.resources <- Wood :: t.resources

let give_wheat t =
  t.resources <- Wheat :: t.resources

(**[remove_resources resource acc lst] loops through resources returns 
   reversed list, which does not matter since resources is a set where order
   does not matter*)
let rec remove_resource (res:r) (acc:r list) (not_rem:bool)= function
  |[]->if(not_rem) then failwith "Does not have resource" 
    else acc
  |h::t->
    (if h = res  && not_rem then remove_resource res acc false t 
     else remove_resource res (h::acc) not_rem t)

(**[take_sheep t] takes a sheep from player [t]*)
let take_sheep t = 
  t.resources <- (remove_resource Sheep [] true t.resources)

(**[take_wheat t] takes a wheat from player [t]*)
let take_wheat t = 
  t.resources <- (remove_resource Wheat [] true t.resources)

(**[take_rock t] takes a rock from player [t]*)
let take_rock t = 
  t.resources <- (remove_resource Rock [] true t.resources)

(**[take_brick t] takes a brick from player [t]*)
let take_brick t = 
  t.resources <- (remove_resource Brick [] true t.resources)

(**[take_wood t] takes a wood from player [t]*)
let take_wood t = 
  t.resources <- (remove_resource Wood [] true t.resources)

let rec half_resources resources len index = 
  match resources with 
  |[]-> failwith "not possible"
  |h::t-> if(len=index) then resources else half_resources t len (index+1) 

let rob_player t = 
  if List.length (t.resources) > 7 then 
    t.resources <- half_resources t.resources ((List.length t.resources)/2) 0 

let player_to_string player = 
  match player.color with 
  |Magenta -> "Magenta"
  |Blue -> "Blue"
  |Green -> "Green"
  |Yellow -> "Yellow"

let resources_to_string player = 
  let rec loop list =
    match list with 
    |[] -> []
    |h::t -> begin 
        match h with
        |Wood -> "Wood" :: loop t
        |Brick -> "Brick" :: loop t
        |Wheat -> "Wheat" :: loop t
        |Sheep -> "Sheep" :: loop t
        |Rock -> "Rock" :: loop t
        |Desert -> "Desert" :: loop t 
      end in loop player.resources

(**[subset lst1 lst2] returns true if lst2 is a subset of lst 1 *)
let rec subset (lst1:r list) (lst2:r list) : bool = 
  match lst2 with 
  |[]-> true
  |h::t-> if(List.mem h lst1) then 
      let lst1 = remove_resource h [] true lst1 in 
      subset lst1 t 
    else false

let build_settlement (player:t) : unit = 
  let resoures_req = [Sheep;Wood;Brick;Wheat] in
  if subset player.resources resoures_req then 
    (take_sheep player;
     take_wood player;
     take_brick player;
     take_wheat player;
     ())
  else 
    failwith "Not enough resources for settlement"

let build_city (player:t) : unit = 
  let resources_req = [Wheat;Wheat;Rock;Rock;Rock] in 
  if subset player.resources resources_req then 
    (
      take_wheat player;
      take_wheat player; 
      take_rock player;
      take_rock player;
      take_rock player;
      ())
  else 
    failwith "not enough resources for a city"

let build_road (player:t) : unit =
  let resources_req = [Wood;Brick] in 
  if subset player.resources resources_req then 
    (take_wood player;
     take_brick player;
     ())
  else 
    failwith "not enough resources for a road"