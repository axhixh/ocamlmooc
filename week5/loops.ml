(* prelude *)
let is_multiple i x = i mod x = 0

(* exercise *)
let output_multiples x n m = 
  for i = n to m do
    if is_multiple i x then 
      begin
        if i > 0 then print_string "," else ();
        print_int i 
      end
    else ()
  done;;

exception ZeroExn;;

let display_sign_until_zero f m =
  try
    for i = 0 to m do
      let x = f i in
      if x > 0 then
        print_endline "positive"
      else if x < 0 then
        print_endline "negative"
      else
        raise ZeroExn
    done
  with
  |_ -> print_endline "zero";;
