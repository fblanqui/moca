(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: cantor_op.ml,v 1.1 2007-11-22 15:35:28 weis Exp $ *)

(* Cantor's ordinals: w^x1.k1 + ... + w^xn.kn with x1 > ... > xn *)

open Cantor;;

let rec bprintf ob = function
  | Zero -> Printf.bprintf ob "0"
  (* beginning of optional code *)
  | Cons (k, Zero, _) ->
      Printf.bprintf ob "%a" Peano.bprintf k
  | Cons (Peano.S Peano.Zero, Cons(Peano.S Peano.Zero, Zero, _), Zero) ->
      Printf.bprintf ob "w"
  | Cons (k, Cons(Peano.S Peano.Zero, Zero, _), Zero) ->
      Printf.bprintf ob "w.%a" Peano.bprintf k
  | Cons (Peano.S Peano.Zero, (Cons(_, Zero, _) as x), Zero) ->
      Printf.bprintf ob "w^%a" bprintf x
  | Cons (k, (Cons(_, Zero, _) as x), Zero) ->
      Printf.bprintf ob "w^%a.%a" bprintf x Peano.bprintf k
  | Cons (Peano.S Peano.Zero, x, Zero) ->
      Printf.bprintf ob "w^(%a)" bprintf x
  | Cons (k, x, Zero) ->
      Printf.bprintf ob "w^(%a).%a" bprintf x Peano.bprintf k
  | Cons (Peano.S Peano.Zero, x, y) ->
      Printf.bprintf ob "w^(%a)+%a" bprintf x bprintf y
  (* end of optional code *)
  | Cons (k, x, y) ->
      Printf.bprintf ob "w^(%a).%a+%a" bprintf x Peano.bprintf k bprintf y
;;

(* -- commutative! -- ordinal addition *)

let rec add = function
  | Zero, x
  | x, Zero -> x
  | x, Cons(l, z, t) -> add (cons(l, z, x), t)
;;

(* multiplication by w^x.k *)

let dot = function
  | Peano.Zero -> fun _x _y -> zero
  | k -> fun x ->
      let rec aux = function
        | Zero as x -> x
        | Cons(l, z, t) -> cons (Peano.mul k l, add (x, z), aux t)
      in aux
;;

(* commutative multiplication *)

let rec mul = function
  | Zero, _
  | _, Zero -> zero
  | Cons(k, x, y), z -> add (dot k x z, mul(y, z))
;;
