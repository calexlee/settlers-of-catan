(* Player to be implemented here *)
type r = Wood | Brick | Sheep | Rock | Wheat
type t = {
  resources: r list;
  point: int;
  (*card_list: *)
  longest_road: bool;
}

let get_points t =
  t.point

let get_resources t =
  t.resources

let give_sheep t =
  { t with
    resources = Sheep :: t.resources
  }
let give_rock t =
  { t with
    resources = Rock :: t.resources
  }

let give_brick t =
  { t with
    resources = Brick :: t.resources
  }

let give_wood t =
  { t with
    resources = Wood :: t.resources
  }

let give_wheat t =
  { t with
    resources = Wheat :: t.resources
  }
