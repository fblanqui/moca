(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2008,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: tfield_math.ml,v 1.13 2010-08-12 07:19:56 weis Exp $ *)

(* Testing Field. *)

open Gentest;;

open Field_math;;

testing "Field_math"
;;

(* Functions that need the regular definition of operators. *)
let rec int_of_expr = function
  | FZero -> 0
  | FOne -> 1
  | FAdd (FOne, t) -> succ (int_of_expr t)
  | FAdd (t, FOne) -> succ (int_of_expr t)
  | FAdd (u, v) -> Pervasives.( + ) (int_of_expr u) (int_of_expr v)
  | FMul (u, v) -> Pervasives.( * ) (int_of_expr u) (int_of_expr v)
  | FInv u -> (* FIXME *) Pervasives.( ~- ) (int_of_expr u)
  | FOpp u -> Pervasives.( ~- ) (int_of_expr u)
;;

let int_neg x = Pervasives.( ~- ) x
;;

(* Now we redefine the usual arithmetic operators to work on the relational type. *)
let ( + ) x y = fAdd (x, y);;
let ( ~+ ) x = fOpp x;;
let ( - ) x y = x + (~+ y);;
let ( * ) x y = fMul (x, y);;
let ( ~* ) x = fInv (x);;
let ( / ) x y = x * (~* y);;

(* Basic constants of the field. *)
let rec ntime n e =
  match n with
  | 0 -> fZero
  | n -> e + ntime (pred n) e
;;

let (zero as _0) = fZero
and (one as _1) = fOne
and (two as _2) = ntime 2 fOne
;;

let rec power e n =
  match n with
  | 0 when e <> _0 -> _1
  | 0 (* when e = _0*) -> failwith "Cannot compute 0 ** 0"
  | 1 -> e
  | n when n > 0 -> e * power e (pred n)
  | n -> ~* (power e (int_neg n))
;;

let ( ** ) = power;;

let square e = e ** 2;;

let rec expr_of_int = function
  | 0 -> _0
  | 1 -> _1
  | n when n > 0 -> ntime n _1
  | n (* when n < 0 *) -> fOpp (expr_of_int (int_neg n))
;;

(* (x + 1) ^ 2 = x * x + 2 * x + 1 *)

let e1 x = square (x + one);;
let e2 x = square x + ntime 2 x + one;;
(*e1 - e2;;*)

testi 0 (e1 _2 = e2 _2)
;;

(* (x + 2) ^ 3 *)
let expression x = power (one + x + one) 3;;

(* (x + 2) ^ 3 - 8 = x^3 + 6 x^2 + 12x = 0 *)
let test_ident x =
  (expression x =
    expr_of_int 8
    + power x 3
    + expr_of_int 6 * (square x)
    + expr_of_int 12 * x)
;;

testi 1 (test_ident _0)
;;
testi 2 (test_ident _1)
;;
testi 3 (test_ident _2)
;;

testi 4
(try (let x = fAdd (fAdd (fZero, fOne), fOne) in
      fMul (x, fInv x) = fOne) with
   | Failure "Division by Absorbent" -> true)
;;

testi 5
(try (let x = fAdd (fAdd (fZero, fOne), fOne) in
      fMul (fInv x, x) = fOne) with
   | Failure "Division by Absorbent" -> true)
;;

(* (-1) * (-1) = 1 *)
testi 6 (~+ _1 * ~+ _1 = _1)
;;

(* (-1) ** 2 = 1 *)
testi 7 ( (~+ _1) ** 2 = _1 )
;;

testi 8
(try
  (let z = fMul (fInv fOne, fOpp fOne) in
   let x = fInv (fAdd (fOne, fZero)) in
   let y = fOpp (fAdd (fZero, fOne)) in
   fMul (z, fAdd (x, y)) = fAdd (fMul (z, x), fMul (z, y)))
 with
 | Failure "Division by Absorbent" -> true)
;;

(* x * (1/x) = 1 with x = 1/(-1) *)
testi 9
 (let x = fInv (fOpp fOne) in
  fMul (fInv x, x) = fOne)
;;

(* x * (1/x) = 1 with x = - (1/(-1)) *)
testi 10
 (let x = fOpp (fInv (fOpp fOne)) in
  fMul (fInv x, x) = fOne)
;;

(* x * (1/x) = 1 with x = - (1/1) *)
testi 11
 (let x = fOpp (fInv fOne) in
  fMul (fInv x, x) = fOne)
;;

(* 1 / (- 0) is an error! *)
testi 12
(try (let y = fInv (fOpp fZero) in
      fInv (fOpp y) = fOpp (fInv y)) with
   | Failure "Division by Absorbent" -> true)
;;

(**
let var0 = var "0"
let var1 = var "1"
let var2 = var "2"

let checf_associative f_constr =
  f_constr (var0, f_constr (var1, var2)) =
   f_constr (f_constr (var0, var1), var2)
;;

let checf_commutative f_constr =
  f_constr (var0, var1) = f_constr (var1, var0)
;;

let checf_idempotent f_constr =
  f_constr (var0, f_constr (var0, var1)) = f_constr (var0, var1) &&
  f_constr (f_constr (var0, var1), var1) = f_constr (var0, var1)
;;

let checf_neutral f_constr neutr =
  f_constr (var0, neutr) = var0 &&
  f_constr (neutr, var0) = var0
;;

let checf_absorbent f_constr abs =
  f_constr (var0, abs) = abs &&
  f_constr (abs, var0) = abs
;;

let checf_inverse f_constr f_inv neutr =
  f_constr (var0, f_inv var0) = neutr &&
  f_constr (f_inv var0, var0) = neutr
;;
*)
(* constants *)
testl 19 (zero <> one);

(* opposite *)
testl 20 (fOpp zero = zero);
testl 21 (fOpp one <> one);

(* add *)
(*
testl 30 (checf_associative add);
testl 31 (checf_commutative add);
testl 32 (checf_neutral add zero);
testl 33 (checf_inverse add opp zero);

(* mul *)
testl 40 (checf_associative mul);
testl 41 (checf_commutative mul);
testl 42 (checf_neutral mul one);
testl 43 (checf_absorbent mul zero)
;;

testl 44
  (mul (add (var0, var1), var2) =
   add (mul (var0, var2), mul (var1, var2)))
;;
testl 45
  (mul (var0, add (var1, var2)) =
   add (mul (var0, var1), mul (var0, var2)))
;;
testl 46 (mul (opp (var0), var1) = opp (mul (var0, var1)))
;;
testl 47 (mul (var0, opp (var1)) = opp (mul (var0, var1)))
;;
*)
