let is_sorted a =
  if Array.length  a < 2 then
    true
  else
    let rec helper a i =
      if i == Array.length a - 1 then
        true
      else if String.compare a.(i) a.(i + 1) >= 0 then
        false
      else
        helper a (i + 1) in
    helper a 0;;

let find dict word =
  if Array.length dict = 0 then
    -1
  else
    let rec helper lower_index higher_index =
      let index = (lower_index + higher_index) / 2 in
      let result = String.compare dict.(index) word in
      if result = 0 then
        index
      else if lower_index = index then (* handle integer division *)
        if (index + 1) == (Array.length dict - 1) && String.compare dict.(index + 1) word = 0 then
          index + 1
        else
          -1
      else if result < 0 then
        helper index higher_index
      else
        helper lower_index index in
    helper 0 ((Array.length dict) - 1);;
