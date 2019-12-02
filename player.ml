(* Player to be implemented here *)
type r = Wood | Brick | Wheat | Sheep | Rock | Desert

type color = Green | Magenta | Yellow | Blue


type port = ThreeToOne of bool | TwoToRes of r


type t = {
  color : color;
  mutable resources: r list;
  mutable points: int;
  (*mutable card_list: *)
  mutable longest_road: bool;
  mutable ports: port list;
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
    ports = [];
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

let give_port t (threeToOne:bool) (res:string) : unit = 
  if threeToOne then 
    t.ports <-  (ThreeToOne true)::t.ports
  else
    match res with  
    |"sheep"-> t.ports <-  (TwoToRes Sheep)::t.ports 
    |"wheat"-> t.ports <-  (TwoToRes Wheat)::t.ports
    |"wood"-> t.ports <-  (TwoToRes Wood)::t.ports
    |"brick"-> t.ports <-  (TwoToRes Brick)::t.ports
    |"rock"-> t.ports <-  (TwoToRes Rock)::t.ports
    |_-> failwith "invalid port"

(**[has_three_to_one_help lst] returns true if a [lst] includes a 
   three to one port*)
let rec has_three_to_one_help (lst:port list) : bool =
  match lst with 
  |[]-> false 
  |h::t-> if h=ThreeToOne true then true else has_three_to_one_help t


let has_three_to_one t : bool = 
  has_three_to_one_help t.ports

(**[has_three_to_one_help lst] returns true if a [lst] includes a 
   three to one port*)
let rec has_two_to_one (lst:port list) (res:string) : bool =
  match lst with 
  |[]-> false 
  |h::t-> let newres = 
            (match res with 
             |"sheep"-> Sheep
             |"wheat"-> Wheat
             |"wood"-> Wood
             |"brick"-> Brick
             |"rock"-> Rock
             |_->failwith"")in 
    if h = TwoToRes newres then true else has_three_to_one_help t

let has_two_to_one t (res:string) : bool = 
  has_two_to_one t.ports res



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

let bank_trade (player:t) (x:int) (res1:string) (y:int) (res2:string) : unit = 
  (for var = x downto 0 do
     match res1 with 
     |"sheep"-> take_sheep player
     |"wheat"-> take_wheat player
     |"wood"-> take_wood player
     |"brick"-> take_brick player
     |"rock"-> take_rock player
     |_-> failwith "Invalid resource"
   done;
   for var2 = y downto 0 do
     match res2 with 
     |"sheep"-> give_sheep player 
     |"wheat"-> give_wheat player
     |"wood"-> give_wood player
     |"brick"-> give_brick player
     |"rock"-> give_rock player
     |_-> failwith "Invalid resource"
   done)

(**[has_trade_res_helper x res lst] returns true if there are [x] entries
   of [res] in [lst]*)
let rec has_trade_res_helper x res lst = 
  if x = 0 then true 
  else has_trade_res_helper (x-1) res (remove_resource res [] true lst)

let has_trade_res (player:t) (x:int) (res:string)= 
  let newres = 
    (match res with 
     |"sheep"-> Sheep
     |"wheat"-> Wheat
     |"wood"-> Wood
     |"brick"-> Brick
     |"rock"-> Rock
     |_->failwith"") in 
  has_trade_res_helper x newres player.resources

let rec half_resources resources len index = 
  match resources with 
  |[]-> failwith "not possible"
  |h::t-> if(len=index) then resources else half_resources t len (index+1) 

let rob_player t = 
  if List.length t.resources > 7 then 
    (t.resources <- (half_resources t.resources ((List.length t.resources)/2) 0);
     ()
    )
  else ()

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
let rec subset lst1 lst2 : bool = 
  match lst2 with 
  |[]-> true
  |h::t-> if(List.mem h lst1) then 
      let lst1 = remove_resource h [] true lst1 in 
      subset lst1 t 
    else false

let can_build_set (player:t) : bool = 
  let resources_req = [Sheep;Wood;Brick;Wheat] in 
  subset player.resources resources_req

let can_build_city (player:t) : bool = 
  let resources_req = [Wheat;Wheat;Rock;Rock;Rock] in 
  subset player.resources resources_req

let can_build_road (player:t) : bool = 
  let resources_req = [Brick;Wood] in 
  subset player.resources resources_req

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
