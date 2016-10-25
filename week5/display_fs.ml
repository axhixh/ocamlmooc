(* prelude *)
type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list

(* exercise *)

let rec print_path path = 
  match path with 
  | [] -> ()
  | h :: s :: rs -> let () = print_string (h ^ "/") in print_path (s :: rs)
  | h :: rs -> let () = print_string h in print_path rs;;

let rec print_file lvl name =
  let rec prefix counter accu =
    if counter = 0 then accu else prefix (counter - 1) ("| " ^ accu) in
  print_string (prefix lvl name);;

let rec print_symlink lvl name path =
  let () = print_file lvl name in
  let () = print_string " -> " in
  print_path path;;

let rec print_dir lvl name =
  print_file lvl ("/" ^ name);;

let print_filesystem root =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec print_filesystem lvl items =
    match items with
    | [] -> ()
    | h :: rs -> match h with
      | (name, File) -> 
        let () =  print_file lvl name in 
        let () = print_newline () in 
        print_filesystem lvl rs
      | (name, Dir f ) -> 
        let () = print_dir lvl name in 
        let () = print_newline () in 
        let () = print_filesystem (lvl + 1) f in
        print_filesystem lvl rs
      | (name, Symlink p) -> 
        let () = print_symlink lvl name p in 
        let () = print_newline() in
        print_filesystem lvl rs
  in
  print_filesystem 0 root ;;

let resolve sym path =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec resolve acc path =
    match path with
    | ".."::rs -> if acc = [] then resolve [] rs else resolve (List.tl acc) rs
    | h::rs -> resolve (h::acc) rs
    | [] -> List.rev acc in
  resolve (List.tl (List.rev sym)) path ;;

let rec file_exists root path =
  let rec find items name =
    match items with
    | [] -> None
    | (name', File) as f :: rs when name = name' -> Some f
    | (name', Dir i) as f :: rs when name = name' -> Some f
    | _ :: rs -> find rs name
  in
  match path with
  | [] -> false
  | h::rs -> match find root h with
    | None -> false
    | Some p -> match p with
      | (_, File) -> rs = []
      | (_, Dir fs) -> file_exists fs rs
      | _ -> false;;

(* version with INVALID *)
let print_filesystem root =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec print_filesystem lvl items parent =
    match items with
    | [] -> ()
    | h :: rs -> match h with
      | (name, File) -> 
        let () =  print_file lvl name in 
        let () = print_newline () in 
        print_filesystem lvl rs parent
      | (name, Dir f ) -> 
        let () = print_dir lvl name in 
        let () = print_newline () in 
        let () = print_filesystem (lvl + 1) f (parent @ [name]) in
        print_filesystem lvl rs parent
      | (name, Symlink p) ->
        let p' = if file_exists root (resolve (parent @ [name]) p) then p else ["INVALID"] in 
        let () = print_symlink lvl name p' in 
        let () = print_newline() in
        print_filesystem lvl rs parent
  in
  print_filesystem 0 root [];;