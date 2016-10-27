(* prelude *)

exception Empty;;

(* exercise *)
let swap ra rb =
  let a = !ra in
  ( ra := !rb; rb := a);;

let update r f =
  let o = !r in
  ( r := f o; o);;

let move l1 l2 =
  match !l1 with
  | [] -> raise Empty
  | h::rs -> (l1 := rs; l2 := h::!l2);;

let reverse l =
  let src = ref l and dst = ref [] in 
  let aux () =
    try
      while true do
        move src dst
      done
    with _ -> ()
  in
  aux ();
  !dst;;