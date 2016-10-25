(* exercise *)
let rotate a =
  let n = Array.length a in 
  if n < 2 then ()
  else
    let v = a.(0) in
    for i = 0 to n-2 do
      a.(i) <- a.(i+1)
    done;
    a.(n-1)<-v ;;

let rotate_by a n =
  let n' = if n > 0 then n else Array.length a + n in
  for i = 0 to n' - 1 do
    rotate a
  done;;