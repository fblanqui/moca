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

(* $Id: txor.ml,v 1.5 2012-06-04 13:01:22 weis Exp $ *)

(* Testing booleans. *)

open Gentest;;

open Xor;;

open List;;

testing "Xor"
;;

let not_mem x l = not (mem x l);;

let and_true_false = band (btrue, bfalse);;
let and_false_true = band (bfalse, btrue);;
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

(* constants *)
testl 00 (btrue <> bfalse);;
testl 01 (btrue <> bvar "x");;

(* bnot *)
testl 10 (btrue = bnot bfalse);;
testl 11 (bnot btrue = bfalse);;
testl 12 (not_mem (bnot var0) [var0; btrue; bfalse]);;

(* band *)
testl 20 (check_associative band);;
testl 21 (check_commutative band);;
testl 22 (check_idempotent band);;
testl 23 (check_absorbent band bfalse);;
testl 24 (check_neutral band btrue);;
testl 25 (band (bxor (var0, var1), var2) = bxor (band (var0, var2),
 band(var1, var2)));;
testl 26 (band (var0, bxor (var1, var2)) = bxor (band (var0, var1), band
 (var0, var2)));;

(* bxor *)
testl 30 (check_associative bxor);;
testl 31 (check_commutative bxor);;
testl 32 (check_neutral bxor bfalse);;
testl 33 (bxor (var0, var0) = bfalse);;
testl 34 (bxor (band (var0, var1), band (var1, var0)) = bfalse);;

(* bor *)
testl 40 (check_commutative bor);;
testl 41 (check_associative bor);;
testl 42 (check_neutral bor bfalse);;
testl 43 (check_absorbent bor btrue);;
testl 44 (check_idempotent bor);;
testl 45 (match bor (var0, var1) with
           | Bor _ -> false
           | _ -> true);;

(* bimply *)
testl 50 (bimply (var0, var1) = bor (bnot var0, var1));;
testl 51 (bimply (bfalse, var0) = btrue);;
testl 52 (bimply (var0, btrue) = btrue);;
testl 53 (bimply (var0, var0) = btrue);;
testl 54 (match bimply (var0, var1) with
          | Bimply _ -> false
          | _ -> true);;

(* bequiv *)
testl 60
  (bequiv (var0, var1) =
   band (bimply (var0, var1), bimply (var1, var0)));;
testl 61 (bequiv (var0, var0) = btrue);;
testl 62 (bequiv (btrue, var0) = var0);;
testl 63 (bequiv (bfalse, var0) = bnot var0);;
testl 64 (match bequiv (var0, var1) with
          | Bimply _ -> false
          | Bequiv _ -> false
          | _ -> true);;
