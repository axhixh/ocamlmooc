(* prelude *)
type report = message list
and message = string * status
and status = Successful | Failed

type 'a result = Ok of 'a | Error of exn

let exn_to_string e = "is provided";;

(* exercise *)
let exec f x =
  try 
    let r = f x in
    Ok r
  with 
  | e -> Error e;;

let compare user reference to_string =
  match (user, reference) with
  | (Ok u, Ok r) -> 
    if u = r then 
      ("got correct value " ^ to_string u, Successful) 
    else 
      ("got unexpected value " ^ to_string u, Failed)
  | (Ok u, Error r) -> ("got unexpected value " ^ to_string u, Failed)
  | (Error u, Ok r) -> ("got unexpected exception " ^ exn_to_string u, Failed)
  | (Error u, Error r) ->
    if u = r then ("got correct exception " ^ exn_to_string u, Successful)
    else ("got unexpected exception " ^ exn_to_string u, Failed);;

let test user reference sample to_string =
  let rec aux results = function
    | 0 -> results
    | counter -> 
      let single_test =
        let s = sample() in compare (exec user s) (exec reference s) to_string in
      aux (results @ [single_test]) (counter - 1)
  in aux [] 10;;