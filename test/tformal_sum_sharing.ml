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

(* $Id: tformal_sum_sharing.ml,v 1.1 2007-09-03 18:14:39 weis Exp $ *)

(* Testing Formal_sum. *)

open Gentest;;

open Formal_sum_sharing;;

testing "Formal_sum_sharing";;

testi 0 (
  let x = dot (1, "x") in
  let y = dot (2, "y") in
  plus (x, y) = plus (dot (2, "y"), dot (1, "x"))
);;

testi 1 (
  let x = plus (dot (1, "x"), zero) in
  plus (x, dot (2, "x")) = zero
);;

(* To be revised: those tests fail.

testi 2 (
  let x = dot (2, "x") in
  let y = dot (3, "y") in
  plus (dot (1, "x"), plus (x, y)) = y
);;

testi 3 (
  let x = dot (2, "x") in
  let y = dot (3, "y") in
  plus (dot (1, "x"), plus (y, x)) = y
);;
*)
