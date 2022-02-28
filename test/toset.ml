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

(* $Id: toset.ml,v 1.3 2007-04-25 14:46:13 weis Exp $ *)

(* Tests for the oset.mlm module of ordered sets. *)
(* Plus is associative and idempotent, Empty is neutral. *)

open Oset;;
open Gentest;;

testing "Ordered sets";;

let ( ++ ) x y = plus (x, y);;

let rec of_list = function
  | [] -> empty
  | x :: l -> singleton x ++ of_list l;;

let s0 = of_list [1; 2; 3];;
let s1 = of_list [3; 2; 1];;

testi 0 (s0 ++ s1 = (s0 ++ empty) ++ (empty ++ s1));;
testi 1 (s1 ++ s0 <> (s0 ++ empty) ++ (empty ++ s1));;
testi 2 (singleton 1 ++ s0 = s0);;
(*

FIXME: this test should succeed ?

testi 3 (singleton 1 ++ s1 = s1);;

*)
