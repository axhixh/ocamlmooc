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

(* -- Part B -------------------------------------------------------------- *)

let compute_distribution l =
  let sorted = List.sort (fun a b -> String.compare a b) l in
  let rec aux accu count words =
    match words with
    | [] -> accu
    | h1::h2::rs when h1 = h2 -> aux accu (count + 1) (h2::rs)
    | h::rs -> aux ((h, (count + 1)) :: accu) 0 rs
  in
  { total = List.length sorted;  amounts = aux [] 0 sorted };;

let build_htable words =
  let words' = ("START" :: words) @ ["STOP"]
  and table = Hashtbl.create 16 in
  let update word next = 
    try
      let o = Hashtbl.find table word in 
      Hashtbl.replace table word (next::o) 
    with _ -> Hashtbl.add table word [next]
  in
  let rec aux words =
    match words with
    | h1::h2::rs -> let () = update h1 h2 in aux (h2::rs)
    | [] -> () 
    | h::rs -> ()
  in
  aux words';
  let result = Hashtbl.create (Hashtbl.length table) in 
  Hashtbl.iter (fun k v -> Hashtbl.add result k (compute_distribution v)) table;
  result;;

let next_in_htable table word =
  let () = Random.self_init() in
  let distribution = Hashtbl.find table word in
  let i = Random.int distribution.total in
  let rec pick sum = function 
    | [(w,v)] -> w
    | (w,v)::rs -> if (sum + v) - i <= 0 then pick (sum + v) rs else w
    | _ -> ""
  in
  pick 0 distribution.amounts;;

(* exists in original template; not sure how it is called *)
let walk table =
  "Replace this string with your implementation." ;;

let walk_htable table =
  let rec aux accu next =
    match next with
    | "START" -> let next' = next_in_htable table next in
      aux accu next'
    | "STOP" -> accu
    | _ -> let next' = next_in_htable table next in
      aux (accu @ [next]) next'
  in aux [] "START" ;;

(* -- Part C -------------------------------------------------------------- *)

let is_char c = 
  (c >= 'A' && c <= 'Z') 
  || (c >= 'a' && c <= 'z') 
  || (c >= '0' && c <= '9')
  || (c >= '\128' && c <= '\255');;

let is_sentence_end c = c = '?' || c = '!' || c = '.';;
let is_punctuation c = 
  c = ';' ||
  c = ',' ||
  c = ':' ||
  c = '-' ||
  c = '"' ||
  c = '\'' ||
  is_sentence_end c;;


let sentences str =
  let slen = String.length str in
  let rec aux s_accu w_accu b i =
    if i = slen then
      let w' = if Buffer.length b > 0 then
          w_accu @ [Buffer.contents b]
        else 
          w_accu in
      let s' = if List.length w' > 0 then
          s_accu @ [w']
        else
          s_accu
      in s'
    else
      let c = String.get str i in
      if is_char c then
        let () = Buffer.add_char b c in
        aux s_accu w_accu b (i + 1)
      else if is_sentence_end c then
        let w' = if Buffer.length b > 0 then
            w_accu @ [Buffer.contents b] @ [String.make 1 c]
          else 
            w_accu @ [String.make 1 c]
        in
        let s' = s_accu @ [w'] in
        Buffer.reset b;
        aux s' [] b (i + 1)
      else if is_punctuation c then
        let w' = if Buffer.length b > 0 then
            w_accu @ [Buffer.contents b] @ [String.make 1 c]
          else
            w_accu @ [String.make 1 c]
        in
        Buffer.reset b;
        aux s_accu w' b (i + 1)
      else
        let w' = if Buffer.length b > 0 then
            w_accu @ [Buffer.contents b]
          else w_accu in
        Buffer.reset b;
        aux s_accu w' b (i + 1)
  in
  aux [] [] (Buffer.create 16) 0;;
  
let rec start pl = 
  match pl with
  | 0 -> []
  | _ -> "START" :: start (pl - 1) ;;

let shift l x =
  match l with 
  | [] -> [x]
  | h::rs -> rs @ [x];;

let build_ptable words pl =
  let starts = start pl in
  let words' =  words @ ["STOP"] in
  let table = Hashtbl.create 16 in
  let update key next =
    try
      let old = Hashtbl.find table key in
      Hashtbl.replace table key (next::old)
    with _ -> Hashtbl.add table key [next]
  in
  let rec aux slice words = 
    match words with
    | h::rs -> 
      let () = update slice h in aux (shift slice h) rs
    | _ -> ()
  in 
  aux starts words';
  let result = Hashtbl.create (Hashtbl.length table) in
  Hashtbl.iter (fun k v -> Hashtbl.add result k (compute_distribution v)) table;
  {prefix_length = pl; table = result};;

let walk_ptable { table ; prefix_length = pl } =
  let rec aux accu next = 
    let word = next_in_htable table next in
    if word = "STOP" then 
      accu
    else
      aux (accu @ [word]) (shift next word)
  in
  aux [] (start pl);;

let rec merge_alist l1 l2 =
  match l1 with
  | [] -> l2
  | (k,v)::rs -> try
      let old = List.assoc k l2 in
      let l2' = List.remove_assoc k l2 in
      merge_alist rs ((k, v + old)::l2')
    with _ -> merge_alist rs ((k,v)::l2);;

let rec merge_ptables tl =
  let append_ptables t1 t2 =
    if t1.prefix_length <> t2.prefix_length then
      raise (Invalid_argument "prefix length don't match")
    else
      let result = Hashtbl.copy t1.table in
      Hashtbl.iter 
        (fun k v ->
           try
             let old = Hashtbl.find result k in 
             Hashtbl.replace result k 
               {total = v.total + old.total; 
                amounts = merge_alist v.amounts old.amounts}
           with Not_found -> Hashtbl.add result k v
        ) 
        t2.table;
      {prefix_length = t1.prefix_length; table = result} 
  in
  match tl with
  | [] -> raise (Invalid_argument "empty list")
  | [t] -> t
  | h1::h2::ts -> merge_ptables ((append_ptables h1 h2)::ts);;