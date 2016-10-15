(* prelude *)
type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;

(* exercise *)
let rec height t = 
  match t with 
  | Empty -> 0
  | Node (Empty, _, Empty) -> 1
  | Node (l, _, r) -> 1 + max (height l) (height r);;

let rec balanced t  = 
  match t with
  | Empty -> true
  | Node(Empty, _, r) -> r = Empty
  | Node(l, _, Empty) -> l = Empty
  | Node (l, _, r) -> balanced l && balanced r;;