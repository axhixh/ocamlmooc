let filter p l =
  List.fold_left (fun a x -> if p x then x :: a else a) [] l;;

let partition p l =
  List.fold_right (fun x (pos,neg) -> if p x then (x::pos,neg) else (pos, x::neg)) l ([],[]);;

let rec sort l = 
    match l with
    | [] -> []
    | h::r -> let p,n = partition (fun x -> x < h) r in sort p @ [h] @ sort n;;