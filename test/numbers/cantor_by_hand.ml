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

(* $Id: cantor_by_hand.ml,v 1.1 2007-11-21 09:50:43 blanqui Exp $ *)

(* Cantor's ordinals: w^x1.k1 + ... + w^xn.kn with x1 > ... > xn *)

open Peano;;

type t =
  | Zero
  | Cons of Peano.t * t * t (* Cons(k,x,y) represents w^x.k + y *)
;;

(* comparison function on terms in normal form *)

let rec compare x y =
  match x, y with
  | Zero, Zero -> 0
  | Zero, _ -> -1
  | _, Zero -> 1
  | Cons(k,x,y), Cons(l,z,t) ->
      (match compare x z with
      | 0 ->
	  (match Pervasives.compare k l with
	  | 0 -> compare y t
	  | n -> n)
      | n -> n);;

(* construction functions *)

let zero = Zero;;

let rec cons = function
  | Peano.Zero, _x, y -> y
  | k, x, (Cons(l,y,z) as t) ->
      (match compare x y with
      | -1 -> cons (l, y, cons (k,x,z))
      | 0 -> cons (Peano.add k l, x, z)
      | _ -> Cons (k, x, t))
  | k, x, y -> Cons (k, x, y)
;;

let rec bprintf ob = function
  | Zero -> Printf.bprintf ob "0"
  (* beginning of optional code *)
  | Cons (k,Zero,_) ->
      Printf.bprintf ob "%a" Peano.bprintf k
  | Cons (Peano.S Peano.Zero,Cons(Peano.S Peano.Zero,Zero,_),Zero) ->
      Printf.bprintf ob "w"
  | Cons (k,Cons(Peano.S Peano.Zero,Zero,_),Zero) ->
      Printf.bprintf ob "w.%a" Peano.bprintf k
  | Cons (Peano.S Peano.Zero,(Cons(_,Zero,_) as x),Zero) ->
      Printf.bprintf ob "w^%a" bprintf x
  | Cons (k,(Cons(_,Zero,_) as x),Zero) ->
      Printf.bprintf ob "w^%a.%a" bprintf x Peano.bprintf k
  | Cons (Peano.S Peano.Zero,x,Zero) ->
      Printf.bprintf ob "w^(%a)" bprintf x
  | Cons (k,x,Zero) ->
      Printf.bprintf ob "w^(%a).%a" bprintf x Peano.bprintf k
  | Cons (Peano.S Peano.Zero,x,y) ->
      Printf.bprintf ob "w^(%a)+%a" bprintf x bprintf y
  (* end of optional code *)
  | Cons (k,x,y) ->
      Printf.bprintf ob "w^(%a).%a+%a" bprintf x Peano.bprintf k bprintf y;;

(* -- commutative! -- ordinal addition *)

let rec add = function
  | Zero, x
  | x, Zero -> x
  | x, Cons(l,z,t) -> add (cons(l,z,x), t)
;;

(* multiplication by w^x.k *)

let dot = function
  | Peano.Zero -> fun _x _y -> Zero
  | k -> fun x ->
      let rec aux = function
	| Zero -> Zero
	| Cons(l,z,t) -> cons (Peano.mul k l, add (x, z), aux t)
      in aux;;

(* commutative multiplication *)

let rec mul = function
  | Zero, _
  | _, Zero -> Zero
  | Cons(k,x,y), t -> add (dot k x t, mul (y, t));;
