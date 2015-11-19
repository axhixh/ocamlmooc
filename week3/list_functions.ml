let rec mem x l =
    match l with
    | [] -> false
    | h::t -> if h == x then true else mem x t;;

let rec append l1 l2 =
    match l2 with
    | [] -> l1
    | h :: t -> append (l1 @ [h]) t;;

let combine l1 l2 =
    let rec aux result cl1 cl2 = 
        match (cl1, cl2) with
        | ([],[]) -> result
        | (h1::t1, h2::t2) -> aux (result @ [(h1,h2)]) t1 t2 in
    aux [] l1 l2;;

let rec assoc l k =
    match l with 
    |[] -> None
    |(s, i)::rs -> if s == k then Some i else assoc rs k;;

