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
    points = 0;
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

let rec half_resources resources len index = 
  match resources with 
  |[]-> failwith "not possible"
  |h::t-> if(len=index) then resources else half_resources t len (index+1) 

let rob_player t = 
  t.resources <- half_resources t.resources ((List.length t.resources)/2) 0 

let player_to_string player = 
  match player.color with 
  |Magenta -> "Magenta"
  |Blue -> "Blue"
  |Green -> "Green"
  |Yellow -> "yellow"
