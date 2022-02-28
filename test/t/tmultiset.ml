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

(* $Id: tmultiset.ml,v 1.5 2010-11-26 00:30:23 weis Exp $ *)

(* Tests for the mset.mlm module of multi sets. *)

open Multiset;;
open Gentest;;

testing "Multiset";;

let ( ++ ) x y = union (x, y);;

(* Injection from a list of elements. *)
let rec multi_of_list = function
  | [] -> empty
  | x :: l -> singleton x ++ multi_of_list l;;

(* Computes the cardinal of a multiset. *)
let rec card = function
  | Empty -> 0
  | Singleton _ -> 1
  | Union (s1, s2) -> card s1 + card s2;;

let s0 = multi_of_list [1; 2; 3];;
let s1 = multi_of_list [1; 2; 3];;

(* Testing neutral element and commutativity. *)
testi 0 (s0 ++ s1 = (s0 ++ empty) ++ (empty ++ s1));;
testi 1 (s1 ++ s0 = (s0 ++ empty) ++ (empty ++ s1));;

(* Testing order irrelevance of generated multisets. *)
let s2 = multi_of_list [3; 2; 1];;
testi 2 (s2 = s1);;

(* Testing that multisets can have more than once each element. *)
let s3 = multi_of_list [1; 1; 2; 3];;
testi 3 (s3 <> s0);;
testi 4 (card s3 = 4);;

(* Testing that even in case of repetition, order is still irrelevant. *)
let s4 = multi_of_list [3; 1; 2; 1];;
testi 5 (s4 <> s0);;
testi 6 (s4 = s3);;
