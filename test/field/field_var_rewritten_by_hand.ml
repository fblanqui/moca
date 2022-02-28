type t =
   | Zero
   | One

   | Inv of t
   | Opp of t
   | Var of string

   | Add of t * t
   (* begin
        associative
        commutative
        neutral (Zero)
        inverse (Opp)
      end *)
   | Mul of t * t
   (* begin
        associative
        commutative
        neutral (One)
        inverse (Inv)
        absorbent (Zero)
        distributive (Add)
        distributive inverse (Opp)
      end *)
;;

let rec add moca_z =
  match moca_z with
  | (moca_x, Zero) -> moca_x
  | (Zero, moca_x) -> moca_x

  | (Opp moca_x, moca_y)
      when moca_x = moca_y -> zero
  | (Opp moca_x, Add (moca_y, moca_z))
      when moca_x = moca_y -> moca_z

  (* Special ordering for right comb of associative commutative operator
     with inverse *)

  (* In fact, the ordering between x and y is regular:
     so this would be done by the regular commutativity rule.
     Is this case mandatory ? *)
(*  | (Opp moca_x as x, Opp moca_y as y) when compare moca_y moca_x < 0 ->
    add (y, x) *)

  (* moca_x is greater than moca_y: x can be deleted with something in moca_z *)
  | (Opp moca_x as x, Add (Opp moca_y as y, moca_z)) when compare moca_y moca_x < 0 ->
    add (y, add (x, moca_z))
  | (Opp moca_x as x, (Add (Opp moca_y, _) as y)) when compare moca_y moca_x >= 0 ->
    Add (x, y)
  | (Opp moca_x as x, Add (moca_y, moca_z)) when compare moca_x moca_y > 0 ->
    (* Add (moca_y, add (x, moca_z)) *)
    add (moca_y, add (x, moca_z))

  | (moca_x, Add (Opp moca_y as y, moca_z)) when compare moca_y moca_x < 0 ->
      add (y, add (moca_x, moca_z))

  (* Same rules commuted arguments *)
  | (moca_x, Opp moca_y)
      when moca_x = moca_y -> zero
  | (moca_x, Add (Opp moca_y, moca_z))
      when compare moca_x moca_y = 0 -> moca_z

  (* Could be useless: commutativity will call add (x, add (y, moca_z)) *)
  | (Add (Opp moca_y as y, moca_z), (Opp moca_x as x)) when compare moca_x moca_y < 0 ->
    add (x, add (y, moca_z))
  (* Could be useless: commutativity will call add (x, add (moca_y, moca_z)) *)
  | (Add (moca_y, moca_z), (Opp moca_x as x)) when compare moca_x moca_y > 0 ->
    (* Add (moca_y, add (x, moca_z)) *)
    add (moca_y, add (x, moca_z))

  (* Associativity without Opp *)
  | (Add (moca_x, moca_y), moca_z) ->
    add (moca_x, add (moca_y, moca_z))

  | (moca_x, (Add (Opp moca_y, _) as y)) when compare moca_y moca_x >= 0 ->
    Add (moca_x, y)

  (* Associativity/Commutativity without Opp *)
  | (moca_x, Add (moca_y, moca_z)) when compare moca_x moca_y > 0 ->
    add (moca_y, (add (moca_x, moca_z)))

  | (moca_x, (Add (moca_y, _) as y)) when compare moca_x moca_y <= 0 ->
    Add (moca_x, y)

   (* Commutativity *)
  | (moca_x, moca_y) when compare moca_x moca_y > 0 -> add (moca_y, moca_x)
  | (moca_x, moca_y) -> Add (moca_x, moca_y)

and one = One

and opp moca_x =
  match moca_x with
  | Opp moca_x -> moca_x
  | Add (moca_x1, moca_x2) -> add ((opp moca_x1), (opp moca_x2))
  | Zero -> zero
  | _ -> Opp moca_x

and var moca_x = Var moca_x

and inv moca_x =
  match moca_x with
  | Inv moca_x -> moca_x
  | Mul (moca_x1, moca_x2) -> mul ((inv moca_x1), (inv moca_x2))
  | Zero -> failwith "Division by absorbent"
  | One -> one
  | _ -> Inv moca_x

and mul moca_z =
  match moca_z with
  | (moca_x, One) -> moca_x
  | (One, moca_x) -> moca_x

  | (_, Zero) -> zero
  | (Zero, _) -> zero

  | (Inv moca_x, moca_y)
      when moca_x = moca_y -> one

 (* moca_x is greater than moca_y: x can be deleted with something in moca_z *)
  | (Inv moca_x as x, Mul (Inv moca_y as y, moca_z)) when compare moca_y moca_x < 0 ->
    mul (y, mul (x, moca_z))
  | (Inv moca_x as x, (Mul (Inv moca_y, _) as y)) when compare moca_y moca_x >= 0 ->
    Mul (x, y)
  | (Inv moca_x as x, Mul (moca_y, moca_z)) when compare moca_x moca_y > 0 ->
    mul (moca_y, mul (x, moca_z))

  | (moca_x, Mul (Inv moca_y as y, moca_z)) when compare moca_y moca_x < 0 ->
    mul (y, mul (moca_x, moca_z))

  (* Same rules commuted arguments *)
  | (moca_x, Inv moca_y)
      when moca_x = moca_y -> one
  | (moca_x, Mul (Inv moca_y, moca_z))
      when compare moca_x moca_y = 0 -> moca_z

  (* Could be useless: commutativity will call mul (x, mul (y, moca_z)) *)
  | (Mul (Inv moca_y as y, moca_z), (Inv moca_x as x)) when compare moca_x moca_y < 0 ->
    mul (x, mul (y, moca_z))
  (* Could be useless: commutativity will call mul (x, mul (moca_y, moca_z)) *)
  | (Mul (moca_y, moca_z), (Inv moca_x as x)) when compare moca_x moca_y > 0 ->
    (* Mul (moca_y, mul (x, moca_z)) *)
    mul (moca_y, mul (x, moca_z))

  | (moca_x, (Mul (Inv moca_y, _) as y)) when compare moca_y moca_x >= 0 ->
    Mul (moca_x, y)

(* Distributivity of Add *)
  | (Add (moca_x1, moca_x2), moca_z) ->
    add (mul (moca_x1, moca_z), mul (moca_x2, moca_z))
  | (moca_z, Add (moca_x1, moca_x2)) ->
    add (mul (moca_z, moca_x1), mul (moca_z, moca_x2))

 (* Distributivity of Opp *)
  | (Opp moca_x1, moca_z) -> opp (mul (moca_x1, moca_z))
  | (moca_z, Opp moca_x1) -> opp (mul (moca_z, moca_x1))

  (* Associativity without Inv *)
  | (Mul (moca_x, moca_y), moca_z) ->
    mul (moca_x, mul (moca_y, moca_z))

  (* Associativity/Commutativity without Inv *)
  | (moca_x, Mul (moca_y, moca_z)) when compare moca_x moca_y > 0 ->
    (* If there is no user rules we have not to call mul again on the top of
       the term: we can simply write
       Mul (moca_y, mul (moca_x, moca_z)) *)
    mul (moca_y, (mul (moca_x, moca_z)))

  | (moca_x, (Mul (moca_y, _) as y)) when compare moca_x moca_y <= 0 ->
    Mul (moca_x, y)

   (* Commutativity *)
  | (moca_x, moca_y)
       when compare moca_x moca_y > 0 ->
    (* If there is no user rules we have not to call mul again:
       since the previous rules are completely symmetrical, the call
       mul (moca_y, moca_x) will not find something to rewrite!
       We can safely write
       Mul (moca_y, moca_x)
       If there are user rules, we must call mul again, since the user rules
       may find something to do if arguments are inverted. *)
    mul (moca_y, moca_x)
  | (moca_x, moca_y) ->
    (* Nothing to rewrite, simply return the normalized multiplication. *)
    Mul (moca_x, moca_y)

and zero = Zero
;;

external eq_t : t -> t -> bool = "%equal"
;;

