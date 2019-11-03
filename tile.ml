type r = Wood | Brick | Wheat | Sheep | Rock | Desert

type t = {
  number:int;
  resource:r;
  mutable robber:bool;
}


let make_tile (number:int) (resource:string) (robber:bool)= 
  {
    number=number;
    resource=
      (match resource with 
       |x when x="wood" -> Wood
       |x when x="brick" -> Brick
       |x when x="sheep" -> Sheep
       |x when x="wheat" -> Wheat
       |x when x="rock" -> Rock
       |x when x="desert" -> Desert
       |_-> failwith "invalid resource"
      );
    robber=robber;
  }

let get_resource (tile: t)= 
  match tile.resource with
  |Wood->"wood"
  |Brick->"brick"
  |Sheep->"sheep"
  |Wheat->"sheep"
  |Rock->"sheep"
  |Desert->"sheep"

let get_number (tile: t)= 
  tile.number