(* prelude
 type queue = int list * int list
 *)

 let is_empty (front, back) = 
     List.length front == 0 && List.length back == 0;;

 let enqueue x (front, back) =
     (front, [x] @ back);;

 let split l =
     let rec aux (front, back) = 
         if List.length back < List.length l / 2 then
             let r::rs = front in
             aux (rs, back @ [r])
         else
             (List.rev front, back) in
     aux (l, []);;

 let dequeue ((front : int list), (back: int list)): (int * queue) =
     match (front, back) with
     | (x::rs, y) -> (x, (rs, y));;
     | ([], y) -> let reversed = List.rev y in
          let x::rs = reversed in (x, (rs, []));;

