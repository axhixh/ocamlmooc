let exchange k =
  let t = k / 10 and o = k mod 10 in (o*10 + t);;

let is_valid_answer (grand_father_age, grand_son_age) =
  grand_son_age * 4 = grand_father_age &&
  (exchange grand_father_age * 3) = (exchange grand_son_age);;

let find answer =
  let (max_gf, min_gs) = answer in
  let rec search cgf cgs =
    if is_valid_answer (cgf, cgs) then
      (cgf, cgs)
    else if cgf = cgs then
      if cgf = max_gf then
        (-1, -1)
      else
        search (cgf + 1) min_gs
    else
      search cgf (cgs + 1)
  in search (min_gs + 1) min_gs;;
