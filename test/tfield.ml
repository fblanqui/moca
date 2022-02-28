(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2012,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: tfield.ml,v 1.3 2012-06-04 13:01:22 weis Exp $ *)

(* Testing Field. *)

open Gentest;;

open Field;;

testing "Field";;

let zero = kZero
and one = kOne;;

let ( + ) x y = kAdd (x, y);;
let ( * ) x y = kProd (x, y);;

let rec ntime n e =
  match n with
  | 0 -> zero
  | n -> e + ntime (pred n) e;;

let rec power n e =
  match n with
  | 0 -> one
  | n -> e * power (pred n) e;;

let sqr = power 2;;

(* (x + 1) ^ 2 = x * x + 2 * x + 1 *)

let _0 = zero and _1 = one and _2 = ntime 2 one;;

let e1 x = sqr (x + one);;
let e2 x = sqr x + ntime 2 x + one;;
(*e1 - e2;;*)

testi 0 (e1 _2 = e2 _2);;

let expr_of_int n = ntime n one;;

let rec int_of_expr = function
  | KZero -> 0
  | KOne -> 1
  | KAdd (KOne, t) -> succ (int_of_expr t)
  | KAdd (t, KOne) -> succ (int_of_expr t)
  | KAdd (u, v) -> Pervasives.( + ) (int_of_expr u) (int_of_expr v)
  | KProd (u, v) -> Pervasives.( * ) (int_of_expr u) (int_of_expr v)
;;

(* (x + 2) ^ 3 *)
let exp x = power 3 (one + x + one);;

(* (x + 2) ^ 3 - 8 = x^3 - 6 x^2 - 12x = 0 *)
let test_ident x =
  (exp x =
    expr_of_int 8
    + power 3 x
    + expr_of_int 6 * (sqr x)
    + expr_of_int 12 * x)
;;

testi 1 (test_ident _0)
;;
testi 2 (test_ident _1)
;;
testi 3 (test_ident _2)
;;

(**
let var0 = var "0"
let var1 = var "1"
let var2 = var "2"

let check_associative f_constr =
  f_constr (var0, f_constr (var1, var2)) =
   f_constr (f_constr (var0, var1), var2)
;;

let check_commutative f_constr =
  f_constr (var0, var1) = f_constr (var1, var0)
;;

let check_idempotent f_constr =
  f_constr (var0, f_constr (var0, var1)) = f_constr (var0, var1) &&
  f_constr (f_constr (var0, var1), var1) = f_constr (var0, var1)
;;

let check_neutral f_constr neutr =
  f_constr (var0, neutr) = var0 &&
  f_constr (neutr, var0) = var0
;;

let check_absorbent f_constr abs =
  f_constr (var0, abs) = abs &&
  f_constr (abs, var0) = abs
;;

let check_inverse f_constr f_inv neutr =
  f_constr (var0, f_inv var0) = neutr &&
  f_constr (f_inv var0, var0) = neutr
;;

(* constants *)
testl 10 (zero <> var0);
testl 11 (zero <> one);
testl 12 (one <> var0);
testl 13 (var0 <> var1);

(* opposite *)
testl 20 (opp zero = zero);
testl 21 (opp one <> one);
testl 22 (opp var0 <> var0);

(* add *)
testl 30 (check_associative add);
testl 31 (check_commutative add);
testl 32 (check_neutral add zero);
testl 33 (check_inverse add opp zero);

(* mul *)
testl 40 (check_associative mul);
testl 41 (check_commutative mul);
testl 42 (check_neutral mul one);
testl 43 (check_absorbent mul zero);
testl 44 (mul (add (var0, var1), var2) = add (mul (var0, var2), mul (var1,
 var2)));
testl 45 (mul (var0, add (var1, var2)) = add (mul (var0, var1), mul (var0,
 var2)));
testl 46 (mul (opp (var0), var1) = opp (mul (var0, var1)));
testl 47 (mul (var0, opp (var1)) = opp (mul (var0, var1)));
*)
