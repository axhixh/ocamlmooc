(* prelude
  type index = Index of int
  *)

  let read (a : int array) index = 
      match index with
      | Index i -> a.(i);;

  let inside (a : int array) index = 
      match index with
      | Index i -> i >= 0 && i < Array.length a;;

  let next index = 
      match index with
      | Index i -> Index (i + 1);;

  let min_index a =
      let rec aux index current_min current_min_index =
          if not (inside a index) then
              current_min_index
          else if read a index < current_min then
              aux (next index) (read a index) index
          else
              aux (next index)
              current_min
              current_min_index in
      aux (Index 1) (read a (Index 0)) (Index 0);;

