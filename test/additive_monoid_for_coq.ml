type monoid =
  | Zero
  | One
  | Add of monoid * monoid
  (* begin
       commutative
       associative
       neutral (Zero)
     end *)
;;

(* Value defined from type monoid *)
let eq = 0
and lt = -1
and gt = 1
;;

let rec compare x y =
  match x, y with
  | Zero, Zero -> eq
  | Zero, _ -> lt
  | One, One -> eq
  | One, Zero -> gt
  | Add (x, y), Add (z, t) ->
      let c = compare x z in
      if c = eq then compare y t else c
  | Add (_, _), _ -> gt
  | _, Add (_, _) -> lt
;;

let rec add moca_z =
  match moca_z with
  | (Add (moca_x, moca_y), moca_z) -> add (moca_x, add (moca_y, moca_z))
  | (Zero, moca_y) -> moca_y
  | (moca_x, Zero) -> moca_x
  | (moca_x, moca_y) -> insert_add moca_x moca_y

and is_redex_add moca_z =
  match moca_z with
  | (Add (_, _), _) -> true
  | (Zero, _) -> true
  | (_, Zero) -> true
  | (moca_x, Add (moca_y, _)) -> compare moca_x moca_y > 0
  | (moca_x, moca_y) -> compare moca_x moca_y > 0

and return_add moca_z =
  match moca_z with
  | (moca_x, moca_y) ->
      if is_redex_add (moca_x, moca_y)
      then add (moca_x, moca_y)
      else Add (moca_x, moca_y)

and insert_add moca_x moca_u =
  match moca_u with
  | Add (moca_z, moca_t) ->
    if compare moca_x moca_z > 0
    then return_add (moca_z, insert_add moca_x moca_t)
    else return_add (moca_x, moca_u)
  | _ ->
    if compare moca_x moca_u > 0
    then return_add (moca_u, moca_x)
    else return_add (moca_x, moca_u)

and one = One

and zero = Zero
;;

(*external eq_monoid : monoid -> monoid -> bool =  "%equal"
;;
*)
