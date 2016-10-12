(* preclude *)
type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3)) 

(* exercise *)  

let my_example = EAdd (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3));;

let rec eval e =
  match e with
  | EInt x -> x
  | EAdd (x, y) -> (eval x) + (eval y)
  | EMul (x, y) -> (eval x) * (eval y);;

let factorize e =
  match e with 
  | EAdd (EMul (a,b), EMul (c,d)) -> if a = c then EMul(a, EAdd(b, d)) else e
  | _ -> e;;

let expand e =
  match e with
  | EMul (a, EAdd (b, d)) -> EAdd(EMul (a,b), EMul (a, d))
  | _ -> e;;

(* this one recursively simplifies; the exercise only expects a single level *)
let rec simplify2 e =
  match e with
  | EMul (a, EInt 0) -> EInt 0
  | EMul (EInt 0, a) -> EInt 0
  | EMul (a, EInt 1) -> simplify2 a
  | EMul (EInt 1, a) -> simplify2 a
  | EAdd (a, EInt 0) -> simplify2 a
  | EAdd (EInt 0, a) -> simplify2 a
  | _ -> e;;

let simplify e =
  match e with
  | EMul (a, EInt 0) -> EInt 0
  | EMul (EInt 0, a) -> EInt 0
  | EMul (a, EInt 1) -> a
  | EMul (EInt 1, a) -> a
  | EAdd (a, EInt 0) -> a
  | EAdd (EInt 0, a) -> a
  | _ -> e;;