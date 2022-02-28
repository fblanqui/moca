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

(* $Id: tgroup_opt.ml,v 1.5 2007-04-25 14:46:13 weis Exp $ *)

(* Testing group_opt.mlm. *)

open Gentest;;
testing "Group_opt";;

open Group_opt;;

let ( + ) x y = add (x, y);;

let ( ~- ) = opp;;

let ( - ) x y = x + ~- y;;

let _0 = d_0;;
let _1 = d_1;;
let two as _2 = _1 + _1;;
let _3 = _2 + _1;;
let _4 = _2 + _2;;
let _5 = _2 + _2 + _1;;
let _6 = _2 + _2 + _2;;
let _7 = _5 + _1 + _1;;
let _8 = _4 + _4;;
let _9 = _2 + _7;;
let _10 = _4 + _4 + _2;;

let rec fib = function
  | D_0 | D_1 -> d_1
  | n -> fib (n - d_1) + fib (n - two);;

let rec int_of = function
  | D_0 -> 0
  | D_1 -> 1
  | Add (x, y) -> Pervasives.(+) (int_of x) (int_of y)
  | n -> Pervasives.succ (int_of (n - d_1));;

let rec cfib = function
  | 0 | 1 -> 1
  | n -> Pervasives.(+) (cfib (Pervasives.pred n)) (cfib (Pervasives.(-) n 2));;

testi 0 (d_1 = (d_1 + d_1) - d_1);;
testi 1 (d_1 = d_1 + (d_1 - d_1));;

testi 2 (two + (two + two) = (two + two) + two);;

testi 3 (_2 + _1 = _1 + _2);;

testi 4 (_7 = _5 + _2);;

testi 5 (_10 - _1 = _9);;
testi 6 (_10 - _2 = _8);;
testi 7 (_10 - _3 = _7);;
testi 8 (_10 - _4 = _6);;
testi 9 (_10 - _5 = _5);;
testi 10 (_10 - (_5 + _1) = _4);;
testi 11 (_10 - (_5 + _1 + _1) = _3);;
testi 12 (_10 - (_5 + _1 + _1 + _1) = _2);;
testi 13 (_10 - (_5 + _1 + _1 + _1 + _1) = _1);;
testi 14 (_10 - (_5 + _2 + _1 + _2) = _0);;
testi 15 (_10 - two - two - two - two - two = _0);; 

testi 16 (int_of (fib _10) = cfib 10);;

(* This test works but is too long.
testi 17 (int_of (fib (_10 + _10)) = cfib 20);; *)

testi 17 (int_of (fib (_5 + _10)) = cfib 15);;


