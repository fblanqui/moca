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

(* $Id: tring_var_sharing.ml,v 1.4 2008-02-13 19:12:43 weis Exp $ *)

(* Testing ring_var_sharing. *)

open Gentest;;

open Ring_var_sharing;;

testing "Ring_var_sharing";;

let ( + ) x y = add (x, y);;
let ( * ) x y = mul (x, y);;
let ( - ) x y = add (x, opp y);;

let s x = add (x, one);;

let _0 = zero;;
let _1 = s _0;;
let _2 = s _1;;
let _3 = s _2;;
let _4 = s _3;;
let _5 = s _4;;

let x = var "x" and y = var "y";;

let rec ntime n e =
  match n with
  | 0 -> zero
  | n -> e + ntime (pred n) e;;

let rec power n e =
  match n with
  | 0 -> one
  | n -> e * power (pred n) e;;

let sqr = power 2;;

let e = (x*x) + (x+x);;

(* (x + 1) ^ 2 = x * x + 2 * x + 1 *)

let e1 = sqr (x + one);;
let e2 = sqr x + ntime 2 x + one;;
(*e1 - e2;;*)

testi 0 (e1 - e2 = zero);;

let expr_of_int n = ntime n one;;

let rec int_of_expr = function
  | Zero -> 0
  | One -> 1
  | Add (_, One, t) -> succ (int_of_expr t)
  | Add (_, t, One) -> succ (int_of_expr t)
  | Opp (_, t) -> ~- (int_of_expr t)
  | Add (_, u, v) -> Pervasives.( + ) (int_of_expr u) (int_of_expr v)
  | _ -> assert false;;

(* (x + 2) ^ 3 *)
let exp = power 3 (one + x + one);;

(* (x + 2) ^ 3 - 8 = x^3 - 6 x^2 - 12x = 0 *)
testi 1
  (exp - (expr_of_int 8)
  - power 3 x
  - expr_of_int 6 * (sqr x)
  - expr_of_int 12 * x = zero);;

