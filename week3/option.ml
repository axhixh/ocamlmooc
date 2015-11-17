let find a w =
    let rec aux idx =
        if idx = Array.length a then
            None
        else if w = a.(idx) then
            Some idx
        else 
            aux (idx + 1) in
    aux 0;;

let default_int opt =
    match opt with
    | Some x -> x
    | None -> 0;;

let merge a b =
    match (a, b) with
    | (None, None) -> None
    | (Some x, None) -> Some x
    | (None, Some x) -> Some x
    | (Some x, Some y) -> Some (x + y);;
