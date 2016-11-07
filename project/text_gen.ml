(* prelude *)
type ltable = (string * string list) list
type distribution =
  { total : int ;
    amounts : (string * int) list }
type htable = (string, distribution) Hashtbl.t
type ptable =
  { prefix_length : int ;
    table : (string list, distribution) Hashtbl.t }

let simple_0 =
  "I am a man and my dog is a good dog and a good dog makes a good man"

let simple_1 =
  "a good dad is proud of his son and a good son is proud of his dad"

let simple_2 =
  "a good woman is proud of her daughter and a good daughter is proud of her mom"

let simple_3 =
  "there is a beer in a fridge in a kitchen in a house in a land where \
   there is a man who has a house where there is no beer in the kitchen"

let multi_1 =
  "A good dad is proud of his son. \
   A good son is proud of his dad."

let multi_2 =
  "A good woman is proud of her daughter. \
   A good daughter is proud of her mom."

let multi_3 =
  "In a land of myths, and a time of magic, \
   the destiny of a great kingdom rests \
   on the shoulders of a young man."

let grimms_travelling_musicians =
  "An honest farmer had once an ass that had been a faithful servant ..."

let grimms_cat_and_mouse_in_partnership =
  "A certain cat had made the acquaintance of a mouse, and ..."

let the_war_of_the_worlds_chapter_one =
  "No one would have believed in the last years ..."

let some_cookbook_sauce_chapter =
  "Wine Chaudeau: Into a lined saucepan put ½ bottle Rhine ..."

let history_of_ocaml =
  "“Caml” was originally an acronym for Categorical ..."

(* exercise *)
(* -- Part A -------------------------------------------------------------- *)

let words str =
  let is_valid c = (c >= 'A' && c <= 'Z') 
                   || (c >= 'a' && c <= 'z') 
                   || (c >= '0' && c <= '9') in
  let rec aux l b i =
    if i = String.length str then 
      if Buffer.length b > 0 then
        l @ [Buffer.contents b]
      else
        l
    else
      let c = String.get str i in
      if is_valid c then
        let () = Buffer.add_char b c in 
        aux l b (i + 1)
      else
        let l' = if Buffer.length b > 0 then l @ [Buffer.contents b] else l in
        Buffer.reset b;
        aux l' b (i + 1)
  in
  aux [] (Buffer.create 16) 0;;


(* List.assoc, List.nth, List.length *)
let build_ltable words =
  let words' = ("START" :: words) @ ["STOP"] in
  let update table word next = 
    try
      let o = List.assoc word table in
      let t' = List.remove_assoc word table in
      (word, next :: o) :: t'
    with
    | _ -> (word, [next])::table
  in
  let rec aux accu words = 
    match words with
    | [] -> accu
    | h1::h2::rs -> aux (update accu h1 h2) (h2::rs)
    | h::rs -> accu
  in
  aux [] words';;

let next_in_ltable table word =
  let () = Random.self_init() in
  let words = List.assoc word table in
  let i = Random.int (List.length words) in
  List.nth words i;;

let walk_ltable table =
  let rec aux accu next =
    match next with
    | "START" -> let next' = next_in_ltable table next in
      aux accu next'
    | "STOP" -> accu
    | _ -> let next' = next_in_ltable table next in
      aux (accu @ [next]) next' 
  in
  aux [] "START";;

