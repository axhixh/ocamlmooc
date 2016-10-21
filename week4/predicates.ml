(* exercise *)
let for_all p l =
  List.fold_left (fun a x -> a && p x) true l;;

let exists p l =
  List.fold_left (fun a x -> a || p x) false l;;

let sorted cmp l =
  match l with
  | [] -> true
  | h::t -> 
    match List.fold_left 
            (fun a x -> match a with 
               | None -> None
               | Some y -> if cmp y x > 0 then None else Some x) 
            (Some h) 
            l 
    with
    | None -> false
    | Some _ -> true;;