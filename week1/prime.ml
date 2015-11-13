let rec gcd n m =
  if n = 0 then
    m
  else if m = 0 then
    n
  else if m > n then
    gcd (m - n)  n
  else
    gcd (n - m)  m
;;

let rec multiple_upto n r =
  if r = 1 then
    false
  else if n mod r = 0 then
    true
  else
    multiple_upto n (r - 1)
;;

let is_prime n =
  let rec check n limit =
    if n = 1 then
      false
    else if limit = 1 then
      true
    else if n mod limit = 0 then
      false
    else
      check n (limit - 1)
  in
  check n (integer_square_root n)
;;
