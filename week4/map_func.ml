(* prelude *)
type 'a tree =
    Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;

(* exercise *)

let wrap l =
  List.map (fun x -> [x]) l;;

let rec tree_map f t = 
  match t with
  | Node (l, v, r) -> Node ((tree_map f l), (f v), (tree_map f r))
  | Leaf v -> Leaf (f v) ;;