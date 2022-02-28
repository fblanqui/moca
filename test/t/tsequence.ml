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

(* $Id: tsequence.ml,v 1.5 2008-02-11 19:37:33 weis Exp $ *)

(* Tests for the sequence.mlm module of sequence of values. *)

open Sequence;;
open Gentest;;

testing "Sequence";;

let ( ++ ) x y = concat (x, y);;

let rec length = function
  | Empty -> 0
  | Element _ -> 1
  | Concat (s1, s2) -> length s1 + length s2;;

let rec of_list = function
  | [] -> empty
  | x :: l -> element x ++ of_list l;;

let to_list =
  let rec loop accu = function
  | Empty -> accu
  | Element x -> x :: accu
  | Concat (s1, s2) -> loop (loop accu s2) s1 in
  loop [];;

let s0 = element 1;;
let s1 = of_list [1; 2; 3];;
let s2 = of_list [1; 2; 3];;

(* Empty is neutral *)
testi 0 (s0 ++ empty = s0);;
testi 1 (s0 ++ empty = empty ++ s0);;
testi 2 (s0 ++ s1 = (s0 ++ empty) ++ (empty ++ s1));;

(* Concat is associative *)
testi 3 (element 1 ++ (element 2 ++ element 3) =
       (element 1 ++ element 2) ++ element 3);;

testi 4 (s1 ++ s2 = of_list (to_list s1 @ to_list s2));;

(* Concat is not commutative, idempotent, nilpotent *)
testi 5 (element 1 ++ element 2 <> element 2 ++ element 1);;
testi 6 (element 1 ++ (element 1 ++ element 2) <> element 1 ++ element 2);;
testi 7 (element 1 ++ (element 1 ++ element 2) <> element 2);;
