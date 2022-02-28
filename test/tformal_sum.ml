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

(* $Id: tformal_sum.ml,v 1.5 2008-01-17 14:33:26 lowenthl Exp $ *)

(* Testing Formal_sum. *)

open Gentest;;

open Formal_sum;;

testing "Formal_sum";;

testi 0 (
  let x = dot (1, "x") in
  let y = dot (2, "y") in
  plus (x, y) = plus (dot (2, "y"), dot (1, "x"))
);;

testi 1 (
  let x = plus (dot (1, "x"), zero) in
  plus (x, dot (2, "x")) = zero
);;

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

(* Tests generated automatically that once failed *)

testi 4
      (let x = plus (plus (dot (2, "35"), dot (10, "0")), dot (37, "34")) in
       let y = plus (plus (dot (9, "35"), dot (7, "27")), dot (22, "5")) in
       plus (x, y) = plus (y, x))
;;

testi 5
(let x = plus (dot (52, "34"), dot (18, "54")) in
 let y = plus (dot (24, "30"), dot (68, "54")) in
 let z = plus (dot (20, "32"), dot (68, "54")) in
 plus (plus (x, y), z) = plus (x, plus (y, z)))
;;
