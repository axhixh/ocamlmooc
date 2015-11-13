let pairwise_distinct (lup, rup, llp, rlp) =
  lup <> rup && lup <> llp && lup <> rlp &&
  rup <> llp && rup <> rlp &&
  llp <> rlp;;

let wellformed (lup, rup, llp, rlp) =
  let (lup_x, lup_y) = lup and
  (rup_x, rup_y) = rup and
  (llp_x, llp_y) = llp and
  (rlp_x, rlp_y) = rlp in
  lup_x < rup_x && lup_x < rlp_x &&
  llp_x < rup_x && llp_x < rlp_x &&
  lup_y > llp_y && rup_y > rlp_y;;

let rotate_point (x, y) =
  (y, -x);;

let is_left p1 p2 =
  let (p1_x, p1_y) = p1 and
  (p2_x, p2_y) = p2 in
  p1_x < p2_x;;

let is_up p1 p2 =
  let (p1_x, p1_y) = p1 and (p2_x, p2_y) = p2 in
  p1_y > p2_y;;

let rec reorder (p1, p2, p3, p4) =
  if wellformed (p1, p2, p3, p4) then
    (p1, p2, p3, p4)
  else
  if is_left p1 p2 then
    if is_left p3 p4 then
      if is_up p1 p3 then
        if is_up p2 p4 then
          (p1,p2, p3,p4)
        else
          reorder (p1, p4, p3, p2)
      else
        reorder (p3, p2, p1, p4)
    else
      reorder (p1, p2, p4, p3)
  else
    reorder (p2, p1, p3, p4);;


let rotate_tetragon (lup, rup, llp, rlp) =
  reorder (rotate_point lup, rotate_point rup, rotate_point llp, rotate_point rlp);;
