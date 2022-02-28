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

(* $Id: tgroup_2.ml,v 1.4 2008-02-11 19:37:33 weis Exp $ *)

(* Testing group.mlm. *)

open Gentest;;

open Group_2;;

testing "Group_2";;

let ( + ) x y = add (x, y);;
let ( ~- ) = opp;;
let ( - ) x y = x + ~- y;;

let _1 = one;;
let _2 = two;;

testi 0
 ((~- _1) + (_1 + _1) = _1)
;;

testi 1
 ((~- _1) + (_2 + _1) <> _2)
;;

let is_wrong = function
  | Add (Zero, _x) | Add (_x, Zero) -> true
  | Two -> true
  | _ -> false
;;

(* (~- _1) + (_2 + _1) is a normal form (cannot simplify the ~- _1 with the
   inner occurrrence of _1) *)
testi 2
  (not (is_wrong ((~- _1) + (_2 + _1))));;

(* Tests generated automatically that once failed *)

testi 3 (let x = opp (add (two, one)) in
         add (x, opp x) = zero)
;;
