let min a =
  let rec min_helper a i current_min =
    if i = Array.length a then
      current_min
    else if a.(i) < current_min then
      min_helper a (i + 1) a.(i)
    else
      min_helper a (i + 1) current_min in
  min_helper a 1 a.(0);;

let min_index a =
  let rec min_helper a i current_min current_index =
    if i = Array.length a then
      current_index
    else if a.(i) < current_min then
      min_helper a (i + 1) a.(i) i
    else
      min_helper a (i + 1) current_min current_index in
  min_helper a 1 a.(0) 0;;

let it_scales =
  "no";;
