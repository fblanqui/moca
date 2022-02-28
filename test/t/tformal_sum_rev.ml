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

(* $Id: tformal_sum_rev.ml,v 1.2 2007-07-31 09:59:12 weis Exp $ *)

(* Testing Formal_sum. *)

open Gentest;;

open Formal_sum_rev;;

testing "Formal_sum_rev";;

testi 0 (
  let x = dot ("x", 1) in
  let y = dot ("y", 2) in
  plus (x, y) = plus (dot ("y", 2), dot ("x", 1))
);;

testi 1 (
  let x = plus (dot ("x", 1), zero) in
  plus (x, dot ("x", 2)) = zero
);;

(* To be revised: those tests fail.

testi 2 (
  let x = dot ("x", 2) in
  let y = dot ("y", 3) in
  plus (dot ("x", 1), plus (x, y)) = y
);;

testi 3 (
  let x = dot ("x", 2) in
  let y = dot ("y", 3) in
  plus (dot ("x", 1), plus (y, x)) = y
);;
*)
