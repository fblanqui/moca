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

(* $Id: tgroup_commutative_1.ml,v 1.3 2012-06-04 13:01:22 weis Exp $ *)

(* Testing group.mlm. *)

open Gentest;;

open Group_commutative_1;;

testing "Group_commutative_1";;

let ( + ) x y = add (x, y);;
let ( ~- ) = opp;;
let ( - ) x y = x + ~- y;;

let _1 = one;;
let two as _2 = _1 + _1;;

testi 0 (one = (one + one) - one);;
testi 1 (one = one + (one - one));;

testi 2 (two + (two + two) = (two + two) + two);;

testi 3 (_2 + _1 = _1 + _2);;

let rec fib = function
  | Zero | One -> one
  | n -> fib (n - one) + fib (n - two);;

let rec int_of = function
  | Zero -> 0
  | One -> 1
  | n -> succ (int_of (n - one));;

let _10 = two + two + two + two + two;;
int_of (fib _10);;

let _20 = _10 + _10;;

(*int_of (fib _20);;*)

