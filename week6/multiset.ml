(* prelude *)
module type MultiSet_S = sig

  (* A multi-set of type ['a t] is a collection of values of
     type ['a] that may occur several times. *)
  type 'a t

  (* [occurrences s x] return the number of time [x] occurs
     in [s]. *)
  val occurrences : 'a t -> 'a -> int

  (* The empty set has no element. There is only one unique
     representation of the empty set. *)
  val empty : 'a t

  (* [insert s x] returns a new multi-set that contains all
     elements of [s] and a new occurrence of [x]. Typically,
     [occurrences s x = occurrences (insert s x) x + 1]. *)
  val insert : 'a t -> 'a -> 'a t

  (* [remove s x] returns a new multi-set that contains all elements
     of [s] minus an occurrence of [x] (if [x] actually occurs in
     [s]). Typically, [occurrences s x = occurrences (remove s x) x -
     1] if [occurrences s x > 0]. *)
  val remove : 'a t -> 'a -> 'a t

end;;

(* exercise *)
module MultiSet : MultiSet_S = struct
  type 'a t = ('a * int) list;;

  let occurrences s x = try
      List.assoc x s
    with _ -> 0;;

  let empty = [];;

  let insert s x = 
    let old_count = occurrences s x in
    (x, (old_count + 1)) :: (List.filter (fun (x1, _) -> x1 != x) s)

  let remove s x = 
    let old_count = occurrences s x in
    if old_count = 0 then s else 
      (x, (old_count - 1)) :: (List.filter (fun (x1, _) -> x1 = x) s)

end;;

let rec list_char ch = match ch with
  | "" -> []
  | ch -> (String.get ch 0 ) :: (list_char (String.sub ch 1 ( (String.length ch)-1) ) )  ;;

let letters word =
  List.fold_left (fun a i -> MultiSet.insert a i) MultiSet.empty (list_char word);;

let anagram word1 word2 =
  let set1 = letters word1 and set2 = letters word2 in
  let words = word1 ^ word2 in 
  let rec aux result = function 
    | -1 -> result
    | i -> let c = String.get words i in 
      let o1 = MultiSet.occurrences set1 c in
      let o2 = MultiSet.occurrences set2 c in
      aux (result && o1 = o2) (i - 1)
  in
  aux true (String.length words - 1);;
