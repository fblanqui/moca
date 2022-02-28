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

(* $Id: tgroup_commutative.ml,v 1.3 2008-06-11 17:30:31 weis Exp $ *)

(* Testing group.mlm. *)

open Gentest;;

open Group_commutative;;

let rec caml_fib = function
  | 0 | 1 -> 1
  | n -> caml_fib (n - 1) + (caml_fib (n - 2))
;;

let rec int_of =
  let rec int accu = function
  | Zero -> accu
  | Gen n -> accu + n
  | Opp n -> accu - (int 0 n)
  | Add (n, m) -> int (int accu n) m in
  int 0;;

testing "Group_commutative";;

let ( + ) x y = add (x, y);;
let ( ~- ) = opp;;
let ( - ) x y = x + ~- y;;

let _1 as one = gen 1;;
let two as _2 = _1 + _1;;

testi 0 (one = (one + one) - one);;
testi 1 (one = one + (one - one));;

testi 2 (two + (two + two) = (two + two) + two);;

testi 3 (_2 + _1 = _1 + _2);;

let rec fib = function
  | Zero | Gen 1 -> one
  | n -> fib (n - one) + fib (n - two);;

let _10 = two + two + two + two + two;;

testi 4 (caml_fib 10 = int_of (fib _10));;

let _20 = _10 + _10;;

(*testi 5 (caml_fib 20 = int_of (fib _20));;*)

