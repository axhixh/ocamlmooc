(* prelude *)
type operation =
    Op of string * operation * operation
  | Value of int

type env = (string * (int -> int -> int)) list

(* initial_env exists *)
let initial_env = [];;

(* exercise *)
let rec lookup_function n = function 
  | [] -> (invalid_arg "lookup_function")
  | (k,f)::rs -> if n = k then f else lookup_function n rs;;

let add_function name op env =
  (name, op) :: env;;

let my_env =
  add_function "min" (fun x y -> if x < y then x else y) initial_env;;

let rec compute env op =
  match op with
  | Value v -> v
  | Op (n, op1, op2) -> let op' = lookup_function n env in
      op' (compute env op1) (compute env op2);;

let rec compute_eff env = function 
  | Value v -> v
  | Op (n, op1, op2) ->
      lookup_function n env (compute_eff env op1) (compute_eff env op2);;