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

(* $Id: tpeano_sharing.ml,v 1.3 2008-02-11 19:37:33 weis Exp $ *)

(* Testing Peano integers. *)

open Gentest;;

open Peano_sharing;;

testing "Peano_sharing";;

let three = succ (succ (succ zero));;

let six = plus (three, three);;

(* x y -> 2 * x + y *)
let affine x y = plus (plus (x, x), y);;

testi 0 (affine three zero = six);;
