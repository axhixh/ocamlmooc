(* prelude *)
let rec equal_on_common l1 l2 = match l1,l2 with
  | [],_ -> true
  | _,[] -> true
  | h1::r1,h2::r2 -> h1=h2 && equal_on_common r1 r2

(* exercise *)

let rec equal_on_common = function l1 -> (function l2 -> 
    if l1 = [] then 
      true
    else if l2 = [] then
      true
    else 
      let h1::r1 = l1 and h2::r2 = l2 in
      if h1 = h2 then equal_on_common r1 r2 else false);;