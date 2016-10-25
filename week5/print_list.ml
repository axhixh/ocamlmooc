(* exercise *)
let rec print_int_list = function
  | [] -> ()
  | h :: rs -> let () = print_int h in
    let () = print_newline () in 
    print_int_list rs;;

let print_every_other k l =
  let print_i i = 
    let () = print_int i in
    print_newline () in
    let rec print counter l' =
      match l' with 
      | [] -> ()
      | h::rs -> let () = if counter = 0 then print_i h else () in
        let c' = (counter + 1) in 
        if c' = k then print 0 rs else print c' rs in
    print 0 l;;

let rec print_list print = function
  | [] -> ()
  | h :: rs -> let () = print h in
    let () = print_newline () in
    print_list print rs;;