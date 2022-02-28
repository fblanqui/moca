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

(* $Id: tpeano.ml,v 1.4 2008-02-11 19:37:33 weis Exp $ *)

(* Testing Peano integers. *)

open Gentest;;

open Peano;;

testing "Peano";;

let three = succ (succ (succ zero));;

let six = plus (three, three);;

let rec peano_normal_form p = match p with
   Zero -> true
 | Succ p1 -> peano_normal_form p1
 | Plus _ -> false
;;

(* x y -> 2 * x + y *)
let affine x y = plus (plus (x, x), y);;

testi 0 (affine three zero = six);;

List.iter (
 fun p -> test (peano_normal_form p)
) [
 zero;
 three;
 six;
 plus (plus (succ zero, plus (three, six)), plus (six, plus (zero, succ (succ
 (plus (six, three))))))
];;
