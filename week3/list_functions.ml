let rec mem x l =
    match l with
    | [] -> false
    | h::t -> if h == x then true else mem x t;;

let rec append l1 l2 =
  match l1 with
  |[] -> l2
  | f::rs -> f :: append rs l2;;

let rec combine l1 l2 =
  match (l1, l2) with
  | ([],[]) -> []
  | (x::xs, y::ys) -> (x,y) :: combine xs ys;;

let rec assoc l k =
    match l with 
    |[] -> None
    |(s, i)::rs -> if s == k then Some i else assoc rs k;;

