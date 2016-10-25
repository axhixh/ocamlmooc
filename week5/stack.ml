(* prelude *)
type stack = int array
exception Full
exception Empty

(* exercise *)
let create size =
  Array.make (size + 1) 0;;

let push buf elt =
  if Array.length buf <= buf.(0) + 1 then
    raise Full
  else
    (buf.(buf.(0) + 1) <- elt; buf.(0) <- buf.(0) + 1);;

let append buf arr =
  let n = Array.length arr in
  for i = n - 1 downto 0 do
    push buf arr.(i)
  done;;

let pop buf =
  if buf.(0) = 0 then
    raise Empty
  else
    let v = buf.(buf.(0)) in
    let _ = buf.(0) <- buf.(0) - 1 in
    v;;