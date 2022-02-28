type 'a t = 
   | Zero
   | Gen of 'a
   | Opp of 'a t
   | Add of 'a t list
(*     
    begin
     associative
     commutative
     neutral left (Zero)
     neutral right (Zero)
     opposite (Opp)
    end
*)
;;


let zero = Zero
;;

let gen x = Gen x
;;

let opp = function
  | Opp x -> x
  | x -> Opp x
;;

let rec myc_add = function
  | [] -> []
  | [x] -> [x]
  | Zero :: xs ->  myc_add xs
  | Opp x as y :: xs -> 
    (try myc_add (del_opp_add x xs)
    with Not_found -> y :: myc_add xs)
  | x :: xs ->
    try myc_add (del_opp_add (opp x) xs)
    with Not_found -> x :: myc_add xs

and del_opp_add x  = function
  | [] -> raise Not_found
  | y :: ys when x = y -> zero :: ys
  | y :: ys -> y :: (del_opp_add x ys)  

and match_add l =
  match myc_add l with
  | [] -> zero
  | [x] -> x
  | (x :: xs) as res -> Add res
;;


let rec cons_add x l =
  match x with
  | Zero -> l
  | Gen _y -> insert x l
  | Opp y ->
    begin
      try delete y l
      with Not_found -> insert x l
    end
  | Add sl ->
     List.fold_right cons_add sl l

and delete elem l =
  match l with
  | [] -> raise Not_found
  | x :: xs when compare x elem = 0 -> xs
  | x :: xs -> x :: delete elem xs

and insert elem l =
   match l with
  | [] -> [elem]
  | x :: xs when compare x elem < 0 -> x :: insert elem xs
  | x :: xs -> elem :: l
;;

let c_add l =
  match 
   List.fold_right cons_add l []
 with
 | [] -> zero
 | [x] -> x
 | l -> Add l
;;

let add l =
  let flat_add = function
  | Add l -> l
  | x -> [x] in
  let l = List.sort Pervasives.compare
           (Listutils.rev_flat_map flat_add l) in
  match_add l
;;

