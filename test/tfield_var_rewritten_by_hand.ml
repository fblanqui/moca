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

(* $Id: tfield_var_rewritten_by_hand.ml,v 1.1 2011-01-24 17:22:38 weis Exp $ *)

(* Testing Field. *)

open Gentest;;

(*open Field_var_rewritten_by_hand;;*)

testing "Field_var_rewritten_by_hand.ml";;

let ( + ) x y = add (x, y);;
let ( * ) x y = mul (x, y);;

let ( ~+ ) x = opp x;;
let ( ~* ) x = inv x;;

let rec ntime n e =
  match n with
  | 0 -> zero
  | n -> e + ntime (pred n) e;;

let rec power n e =
  match n with
  | 0 -> one
  | n -> e * power (pred n) e;;

let ( ** ) x n = power n x;;

let square = power 2;;

(* (x + 1) ^ 2 = x * x + 2 * x + 1 *)

let _0 = zero and _1 = one and _2 = ntime 2 one;;

let e1 x = square (x + one);;
let e2 x = square x + ntime 2 x + one;;
(*e1 - e2;;*)

testi 0 (e1 _2 = e2 _2);;

let expr_of_int n = ntime n one;;

let rec int_of_expr = function
  | Zero -> 0
  | One -> 1
  | Add (One, t) -> succ (int_of_expr t)
  | Add (t, One) -> succ (int_of_expr t)
  | Add (u, v) -> Pervasives.( + ) (int_of_expr u) (int_of_expr v)
  | Mul (u, v) -> Pervasives.( * ) (int_of_expr u) (int_of_expr v)
  | Opp u -> Pervasives.( ~- ) (int_of_expr u)
  | Inv u -> Pervasives.( / ) 1 (int_of_expr u) (* Oups *)
  | Var s -> 0 (* Oups! *)
;;

(* (x + 2) ^ 3 *)
let exp x = power 3 (one + x + one);;

(* (x + 2) ^ 3 - 8 = x^3 - 6 x^2 - 12x = 0 *)
let test_ident x =
  (exp x =
    expr_of_int 8
    + power 3 x
    + expr_of_int 6 * (square x)
    + expr_of_int 12 * x)
;;

testi 1 (test_ident _0)
;;
testi 2 (test_ident _1)
;;
testi 3 (test_ident _2)
;;

(* (x + 2) ^ 3 *)
let expression x = power 3 (one + x + one);;

(* (x + 2) ^ 3 - 8 = x^3 + 6 x^2 + 12x = 0 *)
let test_ident x =
  (expression x =
    expr_of_int 8
    + power 3 x
    + expr_of_int 6 * (square x)
    + expr_of_int 12 * x)
;;

testi 4 (test_ident _0)
;;
testi 5 (test_ident _1)
;;
testi 6 (test_ident _2)
;;

testi 7
(try (let x = add (add (zero, one), one) in
      mul (x, inv x) = one) with
   | Failure "Division by Absorbent" -> true)
;;

testi 8
(try (let x = add (add (zero, one), one) in
      mul (inv x, x) = one) with
   | Failure "Division by Absorbent" -> true)
;;

(* (-1) * (-1) = 1 *)
testi 9 (~+ _1 * ~+ _1 = _1)
;;

(* (-1) ** 2 = 1 *)
testi 10 ( (~+ _1) ** 2 = _1 )
;;

testi 11
(try
  (let z = mul (inv one, opp one) in
   let x = inv (add (one, zero)) in
   let y = opp (add (zero, one)) in
   mul (z, add (x, y)) = add (mul (z, x), mul (z, y)))
 with
 | Failure "Division by Absorbent" -> true)
;;

(* x * (1/x) = 1 with x = 1/(-1) *)
testi 12
 (let x = inv (opp one) in
  mul (inv x, x) = one)
;;

(* x * (1/x) = 1 with x = - (1/(-1)) *)
testi 13
 (let x = opp (inv (opp one)) in
  mul (inv x, x) = one)
;;

(* x * (1/x) = 1 with x = - (1/1) *)
testi 14
 (let x = opp (inv one) in
  mul (inv x, x) = one)
;;

(* 1 / (- 0) is an error! *)
testi 15
(try
   (let y = inv (opp Zero) in
    inv (opp y) = opp (inv y)) with
 | Failure "Division by absorbent" -> true)
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
testl 20 (opp zero = zero);
testl 21 (opp one <> one);

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
