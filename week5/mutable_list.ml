(* prelude *)
type 'a xlist =
  { mutable pointer : 'a cell }
and 'a cell =
  | Nil
  | List of 'a * 'a xlist ;;

let nil () =
  { pointer = Nil } ;;

let cons elt rest =
  { pointer = List (elt, rest) } ;;

exception Empty_xlist ;;

(* exercise *)
let head l = match l.pointer with
  | Nil -> raise Empty_xlist
  | List (a, rest) -> a;;

let tail l = match l.pointer with
  | Nil -> raise Empty_xlist
  | List (a, rest) -> rest;;

let add a l =
  l.pointer <- List (a, { pointer = l.pointer});;

let chop l = match l.pointer with
  | Nil -> raise Empty_xlist
  | List (a, rest) -> l.pointer <- rest.pointer;;

let rec append l l' =
  match l.pointer with
  | Nil -> l.pointer <- l'.pointer
  | List (a, rest) -> append rest l';;

let rec filter p l = match l.pointer with
  | Nil -> ()
  | List (a, rest) -> 
    if p a then 
      filter p rest 
    else
      (l.pointer <- rest.pointer;
       filter p l);;