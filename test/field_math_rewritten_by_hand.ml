(* To test:
ocamlc -c gentest.mli gentest.ml
cp -p field_math_rewritten_by_hand.ml field_math.ml
ocamlc -c field_math.mli
ocamlc -c field_math.ml
ocamlc gentest.cmo field_math.cmo tfield_math.ml
a.out
ocamlc gentest.cmo field_math.cmo field_math_test.ml
a.out
*)
type field =
   | FZero
   | FOne
   | FOpp of field
   | FInv of field
   | FAdd of field * field
   (* begin
        associative
        commutative
        neutral (FZero)
        inverse (FOpp)
      end *)
   | FMul of field * field
   (* begin
        associative
        commutative
        neutral (FOne)
        absorbent (FZero)
        distributive (FAdd)
        inverse (FInv)
      end *)
;;

(* Defining specialized versions of equal and compare primitives.
   This is normally a trivial alias for the corresponding primitives from
   Pervasives. However, in case of maximum sharing, the compare function must
   be much more complex to get rid of the additional field for the hash code
   numbers, which is semantically irrelevant. *)
external eq_field : field -> field -> bool = "%equal"
;;

external compare_field : field -> field -> int = "%compare"
;;

(* Maximum sharing versions of equal and compare. *)
(*
external eq_field : field -> field -> bool = "%eq"
;;

let rec compare_field x y =
  match x with
  | FZero, FZero -> 0
  | FZero, _ -> -1

  | FOne, FZero -> 1
  | FOne, FOne -> 0
  | FOne, _ -> -1

  | FAdd (x1, y1), FAdd (x2, y2) ->
    (match compare_field x1 x2 with
     | 0 -> compare_field y1 y2
     | c -> c)
  | FAdd (_, _), FMul (_, _) -> -1
  | FAdd (_, _), _ -> 1

  | FOpp x, FOpp y -> compare_field x y
  | FOpp _, FZero -> 1
  | FOpp _, FOne -> 1
    (* FOpp x is smaller than any binary operators and unary FInv. *)
  | FOpp _, _ -> -1

(*  | FMul (_, _), FZero -> 1
  | FMul (_, _), FOne -> 1
  | FMul (_, _), FOpp _ -> 1
  | FMul (_, _), FInv _ -> 1
  | FMul (_, _), FAdd _ -> 1*)
  | FMul (x1, y1), FMul (x2, y2) ->
    (match compare_field x1 x2 with
     | 0 -> compare_field y1 y2
     | c -> c)
    (* FMul x is greater than any other operator. *)
  | FMul (_, _), _ -> 1

  | Finv _, FZero -> 1
  | Finv _, FOne -> 1
  | Finv _, Fopp _ -> 1
  | Finv x, Finv y -> compare_field x y
  | Finv _, _ -> -1
;;

*)

let rec fZero = FZero

and fOne = FOne

and fAdd moca_z =
  match moca_z with
    (* Neutral element rules
       Left: g (e, y) -> y
       Right: g (x, e) -> x *)
  | (FZero, moca_y) -> moca_y
  | (moca_x, FZero) -> moca_x

    (* Rules for inverse:
       Left: g (inv x, x) -> a *)
  | (FOpp moca_x, moca_y)
       when compare_field moca_x moca_y = 0 -> fZero
    (* Rules for inverse:
       Right: g (x, inv x) -> a *)
  | (moca_x, FOpp moca_y)
       when compare_field moca_x moca_y = 0 -> fZero

    (* If associative:
       g (inv x, g (y, z)) when x = y -> g (a, z) *)
  | (FOpp moca_x, FAdd (moca_y, moca_z))
       when compare_field moca_x moca_y = 0 ->
    moca_z

    (* Rules to sort combs in increasing order
       independantly of inv applications:
       If associative & commutative:
       g ((inv _ as x), g (y, z)) when y < x -> g (y, g (x, z)) *)
  | (FOpp _ as moca_x, FAdd (moca_y, moca_z))
       when compare_field moca_y moca_x < 0 ->
    fAdd (moca_y, fAdd (moca_x, moca_z))

    (* Rules to simplify inverses inside combs:
       If associative & inverse right:
       g (x, g (inv y, z)) when x = y -> g (a, z) *)
  | (moca_x, FAdd (FOpp moca_y, moca_z))
       when compare_field moca_x moca_y = 0 -> moca_z

    (* Regular associativity rule
       g (g (x, y), z) -> g (x, g (y, z)) *)
  | (FAdd (moca_x, moca_y), moca_z) ->
    fAdd (moca_x, (fAdd (moca_y, moca_z)))

    (* Sorting combs in increasing order, if associative & commutative:
       g (x, g (y, z)) when x > y -> g (y, g (x, z)) *)
  | (moca_x, FAdd (moca_y, moca_z))
       when compare_field moca_x moca_y > 0 ->
    (* Same remark as for Mul *)
    fAdd (moca_y, fAdd (moca_x, moca_z))

    (* Sorting combs in increasing order, if commutative:
       g (x, y) when x > y -> g (y, x) *)
  | (moca_x, moca_y)
       when compare_field moca_x moca_y > 0 ->
    (* Same remark as for Mul *)
    fAdd (moca_y, moca_x)
    (* Default clause: additive normal form
       fg (x, y) -> g (x, y) *)
  | (moca_x, moca_y) -> FAdd (moca_x, moca_y)

and fOpp moca_x =
  match moca_x with
  | FZero -> fZero
  | FOpp moca_x -> moca_x
  | FAdd (moca_x1, moca_x2) -> fAdd (fOpp moca_x2, fOpp moca_x1)
  | _ -> FOpp moca_x

and fMul moca_z =
  match moca_z with
  | (FZero, _) -> fZero
  | (_, FZero) -> fZero
  | (FOne, moca_y) -> moca_y
  | (moca_x, FOne) -> moca_x

  | (FMul (moca_x, moca_y), moca_z) ->
    fMul (moca_x, fMul (moca_y, moca_z))

  | (FInv moca_x, moca_y)
       when compare_field moca_x moca_y = 0 ->
    fOne
  | (FInv moca_x, FMul (moca_y, moca_z))
       when compare_field moca_x moca_y = 0 ->
    moca_z
  | (FInv moca_x as x, FMul (moca_y, moca_z))
       when compare_field moca_y x < 0 ->
    fMul (moca_y, fMul (x, moca_z))

  | (moca_x, FInv moca_y)
       when compare_field moca_x moca_y = 0 ->
    fOne
  | (moca_x, FMul (FInv moca_y, moca_z))
       when compare_field moca_x moca_y = 0 -> moca_z
  | (moca_x, FMul (FInv moca_y, moca_z))
       when compare_field moca_y (fInv moca_x) < 0 ->
    fMul (moca_y, fMul (fInv moca_x, moca_z))

  | (FAdd (moca_x1, moca_x2), moca_z) ->
    fAdd (fMul (moca_x1, moca_z), fMul (moca_x2, moca_z))
  | (moca_z, FAdd (moca_x1, moca_x2)) ->
    fAdd (fMul (moca_z, moca_x1), fMul (moca_z, moca_x2))
  | (FOpp moca_x1, moca_z) -> fOpp (fMul (moca_x1, moca_z))
  | (moca_z, FOpp moca_x1) -> fOpp (fMul (moca_z, moca_x1))

  | (moca_x, FMul (moca_y, moca_z))
       when compare_field moca_x moca_y > 0 ->
    (* If there is no user rules we have not to call fMul again on the top of
       the term: we can simply write
       FMul (moca_y, fMul (moca_x, moca_z)) *)
    fMul (moca_y, fMul (moca_x, moca_z))
  | (moca_x, moca_y)
       when compare_field moca_x moca_y > 0 ->
    (* If there is no user rules we have not to call fMul again:
       since the previous rules are completely symmetrical, the call
       fMul (moca_y, moca_x) will not find something to rewrite!
       We can safely write
       FMul (moca_y, moca_x)
       If there are user rules, we must call fMul again, since the user rules
       may find something to do if arguments are inverted. *)
    fMul (moca_y, moca_x)
  | (moca_x, moca_y) ->
    (* Nothing to rewrite, simply return the normalized multiplication. *)
    FMul (moca_x, moca_y)

and fInv moca_x =
  match moca_x with
  | FOne -> fOne
  | FZero -> raise (Failure "Division by Absorbent")
  | FInv moca_x -> moca_x
  | FMul (moca_x1, moca_x2) -> fMul (fInv moca_x2, fInv moca_x1)
  | FOpp moca_x1 -> fOpp (fInv moca_x1)
  | _ -> FInv moca_x
;;
