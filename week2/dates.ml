(* prelude
type date =
  { year : int; month : int; day : int;
    hour : int; minute : int }

let the_origin_of_time =
  { year = 1; month = 1; day = 1;
    hour = 0; minute = 0 }
*)
let wellformed date =
  date.year >= 1 &&
  date.month >= 1 && date.month <= 5 &&
  date.day >= 1 && date.day <= 4 &&
  date.hour >= 0 && date.hour <= 2 &&
  date.minute >= 0 && date.minute <=1;;

let minutes_in_hour = 2;;
let minutes_in_day = minutes_in_hour * 3;;
let minutes_in_month = minutes_in_day * 4;;
let minutes_in_year = minutes_in_month * 5;;

let of_int minutes =
  let year = minutes / minutes_in_year and
  remaining = minutes mod minutes_in_year in
  let month = remaining / minutes_in_month and
    remaining = remaining mod minutes_in_month in
  let day = remaining / minutes_in_day and
    remaining = remaining mod minutes_in_day in
  let hour = remaining / minutes_in_hour and
    minute = remaining mod minutes_in_hour in
  { year = year + 1;
    month = month + 1;
    day = day + 1;
    hour = hour;
    minute = minute};;

let next date =
  let minutes = date.minute +
                date.hour * minutes_in_hour +
                (date.day - 1) * minutes_in_day +
                (date.month - 1) * minutes_in_month +
                (date.year - 1) * minutes_in_year in
  of_int (minutes + 1);;
