(* prelude *)
type int_ff = int -> int

(* exercise *)
let rec compose = function 
  | [] -> (function x -> x)
  | [f] -> (function x -> f x)
  | f::fs -> (function x -> f ((compose fs) x));;

let rec fixedpoint f start delta =
  let y = f start in
  if abs_float (y -. f y) < delta then
    y
  else
    fixedpoint f y delta;;