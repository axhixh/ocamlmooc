(* prelude - include db.ml *)

let proof_of_bug =
  [| {code = 0; contact = {name="a";phone_number=(1,2,3,4)}};
     {code = 0; contact = {name="b";phone_number=(1,2,3,4)}};
     {code = 1; contact = {name="a";phone_number=(1,2,3,4)}};
     {code = 2; contact = {name="b";phone_number=(1,2,3,4)}} |] ;;

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let rec aux current_db i =
      if i == db.number_of_contacts then
        (true, current_db, contact)
      else
      if db.contacts.(i).name = contact.name then
        aux current_db (i + 1)
      else
        let (_,db',_) = insert current_db db.contacts.(i) in
        aux db' (i + 1) in
    aux {
      number_of_contacts = 0;
      contacts = Array.make (Array.length db.contacts) nobody
    } 0;;

let update db contact =
  let (status, _, _) = search db contact in
  if not status then insert db contact
  else
    let cells i =
      if db.contacts.(i).name = contact.name then
        contact
      else
        db.contacts.(i) in
    let db' = {
      number_of_contacts = db.number_of_contacts;
      contacts = Array.init (Array.length db.contacts) cells
    }
    in
    (true, db', contact);;

let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;
