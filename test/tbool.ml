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

(* $Id: tbool.ml,v 1.12 2012-06-04 13:01:22 weis Exp $ *)

(* Testing Booleans. *)

open Gentest;;

open Bool;;

testing "Bool";;

let bimply p q = bor (bnot p, q);;

let bequiv p q = band (bimply p q, bimply q p);;

let tauto t = t = btrue;;

let p = btrue and q = bfalse and r = btrue;;

testi 0 (not (tauto (bimply p q)));;
testi 1 (tauto (bimply p p));;
testi 2 (tauto (bimply q q));;
testi 3 (tauto (bimply q q));;
testi 4
  (tauto (bimply (bimply p (bimply q r)) (bimply (bimply p q) r)));;


let var0 = bvar "0";;
let var1 = bvar "1";;
let var2 = bvar "2";;

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
testi 5 (btrue <> bfalse);
testi 6 (btrue <> bvar "x");

(* bnot *)
testi 7 (btrue = bnot bfalse);
testi 8 (bnot btrue = bfalse);

(* band *)
testi 9 (check_associative band);
testi 10 (check_commutative band);
testi 11 (check_idempotent band);
testi 12 (check_absorbent band bfalse);
testi 13 (check_neutral band btrue);
testi 14 true (*check_inverse band bnot bfalse*);

(* bor *)
testi 15 (check_commutative bor);
testi 16 (check_associative bor);
testi 17 (check_neutral bor bfalse);
testi 18 (check_absorbent bor btrue);
testi 19 (check_idempotent bor);
testi 20 true (*check_inverse bor bnot btrue*);

(* Tests generated automatically that once failed *)
(* test the inverse relations *)
(*testi 21
      (let x = band (bnot (bvar "2"), bvar "10") in
       band (x, bnot x) = bfalse)
;;

testi 22
      (let x = bnot (band (bvar "8", bvar "21")) in
       band (bnot x, x) = bfalse)
;;

testi 23 (let x = bor (bnot (bvar "1"), bvar "20") in
          bor (x, bnot x) = btrue)
;;

testi 24
      (let x = bor (bnot (bvar "14"), bor (bvar "9", bvar "15")) in
       bor (x, bnot x) = btrue)
;;

testi 25 (let x = bor (bvar "13", bvar "5") in
          bor (bnot x, x) = btrue)
;;*)
