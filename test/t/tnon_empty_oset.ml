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

(* $Id: tnon_empty_oset.ml,v 1.1 2008-03-28 08:13:29 weis Exp $ *)

(* Tests for the non_empty_oset.mlm module of non empty ordered sets. *)
(* Plus is associative and idempotent, Empty is neutral. *)

open Non_empty_oset;;
open Gentest;;

testing "Non empty ordered sets";;

let ( ++ ) x y = plus (x, y);;

let rec of_list = function
  | [x] -> singleton x
  | x :: l -> singleton x ++ of_list l
  | [] -> failwith "Non empty ordered sets only";;

let s0 = of_list [1; 2; 3];;
let s1 = of_list [3; 2; 1];;

testi 0 (s0 = (s0 ++ s0));;
testi 1 (s1 ++ s0 <> (s0 ++ s1));;
testi 2 (singleton 1 ++ s0 = s0);;
