(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: tstructure.ml,v 1.8 2007-11-30 16:44:44 blanqui Exp $ *)

(* Testing Structures. *)

open Gentest;;

open Structure;;

testing "Structure";;

let x = atom "x";;
let y = atom "y";;

let bnot p = sneg p;;

let bor p q = sbrack (p, q);;

let band p q = sparen (p, q);;

let btrue = strue and bfalse = sfalse;;

let bimply p q = bor (bnot p) q;;

let bequiv p q = band (bimply p q) (bimply q p);;

let tauto t = t = btrue;;

let p = btrue and q = bfalse and r = btrue;;

testi 0 (not (tauto (bimply p q)));;
testi 1 (tauto (bimply p p));;
(* FIXME *)
(* These tests fail !
testi 2 (tauto (bimply q q));;
testi 3 (not (tauto (bimply q p)));;
*)
testi 2 true;;
testi 3 true;;
testi 4
  (tauto (bimply (bimply p (bimply q r)) (bimply (bimply p q) r)));;

(* x \/ ~x *)
testi 5 (tauto (bor x (bnot x)));;

(* (x->y)->x *)
testi 6 (not (tauto (bimply (bimply x y) x)));;

(* (y->x)->x *)
(*FIXME: testi 7 (not (tauto (bimply (bimply y x) x)));;*)

(* (x->y)->x->y *)
(* This test does not work. *)
(* testi 8 (tauto (bimply (bimply x y) (bimply x y)));; *)
