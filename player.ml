(* Player to be implemented here *)
type r = Wood | Brick | Sheep | Rock | Wheat
type t = {
  mutable resources: r list;
  mutable point: int;
  (*mutable card_list: *)
  mutable longest_road: bool;
}

let get_points t =
  t.point

let get_resources t =
  t.resources

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
