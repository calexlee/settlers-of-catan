type r = Wood | Brick | Wheat | Sheep | Rock | Desert

type t = {
  number:int;
  resource:r;
  robber:bool;
}


let make_tile (number:int) (resource:string) (robber:bool)= 
  {
    number=number;
    resource=
      (match resource with 
       |x when x="wood" -> Wood
       |x when x="brick" -> Brick
       |x when x="sheep" -> Wheat
       |x when x="wheat" -> Wheat
       |x when x="rock" -> Rock
       |x when x="desert" -> Desert
       |_-> failwith "invalid resource"
      );
    robber=robber;
  }
