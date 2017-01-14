let rec concat l = 
  match l with
  [] -> ""
  | h::t -> h ^ (concat t)

(*
 * clone 9 5 -> [9;9;9;9]
 * clone 9 0 -> []
 *)

let rec clone z n = 
  match n with
  0 -> []
  | _ -> z :: (clone z (n-1))

let rec every_other l = 
  match l with
  [] -> []
  | [_] -> []
  | e1::e2::t -> h2::(every_second t)

let rec contains e l =
  match l with
  [] -> false
  | first::rest ->
    if first = e then true
    else (contains e rest)

let rec delete_element e l =
  match l with
  [] -> []
  | first::rest ->
    if first = e then
      delete_element e rest
    else
      first::(delete_element e rest)

(* check that each element in l1 occurs in l2 *)
let rec contains_list l1 l2 = 
  match l1 with 
  [] -> true
  | first::rest -> (contains first l2) && (contains_list rest l2)

let range from til step = 
  if from <= til then
    from::(range (from+step) til step)
  else []

