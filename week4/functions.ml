(* exercise *)

let rec last_element = function 
  | [] -> (invalid_arg "last_element")
  | [x] -> x
  | _::xs -> last_element xs;;

let rec is_sorted = function 
  |[] | [_] -> true
  | x::y::z when x < y ->  is_sorted (y::z)
  | _ -> false;;
