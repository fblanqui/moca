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

(* $Id: tfield_var_list.ml,v 1.1 2010-12-20 17:53:48 weis Exp $ *)

(* Testing a ring structure with variables. *)

(* To test interactively, use:

$ ./ocaml_test ring_var_rewritten_by_hand
+ ocamlc gentest.mli
+ ocamlc gentest.ml
+ ocamlc ring_var_rewritten_by_hand.ml
+ ledit -h /home/beaujolais/weis/.ocaml-history -x ocaml \
  gentest.cmo ring_var_rewritten_by_hand.cmo

        Objective Caml version 3.11.1+dev6 (2009-05-20)

# #use "tring_var_rewritten_by_hand.ml";;
*)

(* #use "gentest.ml";;*)
open Gentest;;

open Field_var_list;;

testing "Field_var_rewritten_by_hand"
;;

let ( ~- ) x = opp x;;
let ( + ) x y = add [x; y];;
let ( * ) x y = mul [x; y];;
let ( - ) x y = add [x; opp y];;

let x = var "x" and y = var "y";;

let rec ntime n e =
  match n with
  | 0 -> zero
  | n -> e + ntime (pred n) e
;;

let rec power n e =
  match n with
  | 0 -> one
  | n -> e * power (pred n) e
;;

let sqr = power 2;;

let expr_of_int n = ntime n one;;

let rec int_of_expr = function
  | Zero -> 0
  | One -> 1
  | Add [One; t] -> succ (int_of_expr t)
  | Add [t; One] -> succ (int_of_expr t)
  | Opp t -> Pervasives.( ~- ) (int_of_expr t)
  | Add [u; v] -> Pervasives.( + ) (int_of_expr u) (int_of_expr v)
  | _ -> assert false
;;

(* The trivial tests. *)

let g1 = one + y ;;
let g2 = x + g1 ;;
let g3 = one - y;;

testl (-1) (g3 + g2 + g1 = one + one + one + x + y);;

(* We use the algebraic identity
   (x + 1) ^ 2 = x * x + 2 * x + 1 *)


(* We name e1 the lhs expression (x + 1) ^ 2
   and e2 the rhs expression x ^2 + 2 * x + 1. *)
let e1 = sqr (x + one);;
let e2 = sqr x + ntime 2 x + one;;

(* Clearly we should have e1 = e2 ! *)
testl 0 (e1 = e2)
;;

(* Since e1 = e2, we must have e1 - e2 = zero *)
testl 1 (e1 - e2 = zero)
;;

(* Since e1 - e2 = zero, we must have e2 - e1 = zero *)
testl 2 (e2 - e1 = zero)
;;

prerr_endline "test 3";;

let x2 = x * x;;
let x1 = x + one;;

(*#trace add;;
#trace mul;;
"x1 * x2";;*)

(* (x + 2) ^ 3 *)
let e3 = power 3 (one + x + one);;

prerr_endline "power computed";;

(* (x + 2) ^ 3 - 8 = x^3 - 6 x^2 - 12x = 0 *)
(*
testl 3
  (e3 - (expr_of_int 8)
  - power 3 x
  - expr_of_int 6 * (sqr x)
  - expr_of_int 12 * x = zero)
;;
*)

(* Testing with variables. *)

prerr_endline "var";;

let var0 = var "0";;
let var1 = var "1";;
let var2 = var "2";;

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
testl 10 (zero <> var0);;
testl 11 (zero <> one);;
testl 12 (one <> var0);;
testl 13 (var0 <> var1);;

(* opposite *)
testl 20 (opp zero = zero);;
testl 21 (opp one <> one);;
testl 22 (opp var0 <> var0);;

let mul (x, y) = mul [x; y]
and add (x, y) = add [x; y]
;;

(* add *)
testl 30 (check_associative add);;
testl 31 (check_commutative add);;
testl 32 (check_neutral add zero);;
testl 33 (check_inverse add opp zero);;

(* mul *)
testl 40 (check_associative mul);;
testl 41 (check_commutative mul);;
testl 42 (check_neutral mul one);;
testl 43 (check_absorbent mul zero);;
prerr_endline "couciou";;

testl 44 (mul (add (var0, var1), var2) =
          add (mul (var0, var2), mul (var1, var2)));;
testl 45 (mul (var0, add (var1, var2)) =
          add (mul (var0, var1), mul (var0, var2)));;

testl 46 (mul (opp (var0), var1) = opp (mul (var0, var1)));;
testl 47 (mul (var0, opp (var1)) = opp (mul (var0, var1)));;



(* The hard ones *)
testl 100
 (try
    let x0 = add (var "x", var "y") in
    let x2 = add (zero, mul (var "a", var "b")) in
    mul (x0, add (x0, x2)) =
    add (mul (x0, x0), mul (x0, x2))
  with
  | Failure "Division by Absorbent" -> true)
;;

let add_list l =
  List.fold_left (fun accu t -> add (t, accu)) zero l
and mul_list l =
  List.fold_left (fun accu t -> mul (t, accu)) one l
;;

let x0 = opp (add_list [var "30"; var "28"; ]);;

let x01 = var "46";;
let x2 = var "z"
;;
 let x21 = var "baz"
;;
let x4 = mul_list [ x0; add_list [ x01; x21;]; x2; ];;
let x5 =
add_list [
      mul_list [x0; x01; x2];
      mul_list [x0; x21 ; x2];
    ]
;;
testl 110
 (try
    x4 = x5
  with
  | Failure "Division by Absorbent" -> true)
;;
