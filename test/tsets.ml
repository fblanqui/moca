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

(* $Id: tsets.ml,v 1.7 2012-06-04 13:01:22 weis Exp $ *)

(* Tests for the rset.mlm module of regular sets. *)
(* Plus is associative, commutative, and idempotent, Empty is neutral. *)

open Gentest;;

testing "Sets";;

open Sets;;

let ( ++ ) x y = union (x, y);;

let rec of_list = function
  | [] -> empty
  | x :: l -> singleton x ++ of_list l;;

let s0 = of_list [1; 2; 3];;
let s1 = of_list [3; 2; 1; 2; 3];;

let sing_a = singleton "a";;
let sing_b = singleton "b";;
let sing_c = singleton "c";;

testi 0 (s0 != s1);;

testi 1 (s0 ++ s1 = (s0 ++ empty) ++ (empty ++ s1));;
testi 2 (s1 ++ s0 = (s0 ++ empty) ++ (empty ++ s1));;

(* Base cases *)
testi 3 (singleton 1 <> empty);;
testi 4 (singleton 1 <> singleton 2);;

(* Union *)
testi 5 (sing_a ++ sing_b = sing_b ++ sing_a);;
testi 6 (sing_a ++ (sing_b ++ sing_c) = (sing_a ++ sing_b) ++ sing_c);;
testi 7 (sing_a ++ empty = sing_a);;
testi 8 (empty ++ sing_a = sing_a);;
testi 9 (sing_a ++ (sing_a ++ sing_b) = sing_a ++ sing_b);;
testi 10 ((sing_a ++ sing_b) ++ sing_b = sing_a ++ sing_b);;
