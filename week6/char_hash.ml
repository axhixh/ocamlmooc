(* prelude *)
module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
end

(* exercise *)
module CharHashedType =
struct 
  type t = char
  let equal c1 c2 = Char.code c1 = Char.code c2 
  let hash c = Char.code c
end

module CharHashtbl = Hashtbl.Make(CharHashedType)

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t =
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table

  let empty () = Trie (None, CharHashtbl.create 0)

  let lookup trie w =
    let rec aux t idx = 
      let Trie (v, table) = t in
      if idx = String.length w then
        v
      else
        try
          let t' = CharHashtbl.find table (String.get w idx) in
          aux t' (idx + 1)
        with _ -> None
    in
    aux trie 0 

  let rec insert trie w v =
    let Trie(v', m) = trie in
    if w = "" then
      Trie(Some v, m)
    else 
      let c = String.get w 0
      and rest = String.sub w 1 ((String.length w) - 1) in
      let t' = 
        try
          CharHashtbl.find m c
        with _ -> empty ()
      in
      let m' = CharHashtbl.copy m in
      let _ = CharHashtbl.replace m' c (insert t' rest v) in
      Trie (v', m')
end
