(* prelude *)
type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
        [('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
         ('t',
          Trie (None,
                [('e',
                  Trie (None,
                        [('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
                         ('a', Trie (Some 3, []))]));
                 ('o', Trie (Some 7, []))]));
         ('A', Trie (Some 15, []))])

(* exercise *)
let children_from_char m c =
  let rec find idx =
    if idx < 0 then
      None
    else
      let (k, v) = List.nth m idx in
      if k = c then
        Some v
      else 
        find (idx - 1) in
  find (List.length m - 1)
;;

let update_children m c t =
  let rec update accu idx =
    if idx < 0 then
      accu
    else
      let (k, v) = List.nth m idx in
      if k = c then
        update ((c, t) :: accu) (idx - 1)
      else
        update ((k,v) :: accu) (idx - 1)
  in
  match children_from_char m c with
  | None -> m @ [(c, t)]
  | Some _ -> update [] (List.length m - 1);;


let lookup trie w =
  let rec find t idx =
    let Trie(value, children) = t in
    if idx = String.length w then
      value
    else
      match (children_from_char children (String.get w idx)) with
      | None -> None
      | Some t' -> find t' (idx + 1)
  in
  find trie 0;;

let generate i w v =
  let rec gen idx =
    if String.length w = idx then
      []
    else
      let v' = if String.length w - 1 = idx then Some v else None in
      [((String.get w idx), Trie(v', gen (idx + 1)))]
  in
  gen i;;

let insert2 trie w v =
  let rec put children idx =
    if String.length w = idx then
      children
    else
        match children_from_char children (String.get w idx) with
        | None -> generate idx w v
        | Some t' -> children
      
  in
  let Trie(o, c) = trie in Trie(o, put c 0);;


let rec insert trie w v =
  let slen = String.length w
  and Trie(v',m) = trie in
  if slen = 0 then Trie(Some v, m)
  else
    let c = String.get w 0
    and s = String.sub w 1 (slen - 1) in
    let ot = children_from_char m c in
    match ot with
    | (Some t) -> Trie (v', update_children m c (insert t s v))
    | None -> Trie (v', update_children m c (insert empty s v));;