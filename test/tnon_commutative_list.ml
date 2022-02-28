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

(* $Id: tnon_commutative_list.ml,v 1.16 2012-06-04 13:01:22 weis Exp $ *)

(* Testing properties of non commutativity. *)

open Gentest;;

open Non_commutative_list;;

testing "Non_commutative_list";;

(* Permutations not equivalent *)
testl 0 (
  let x = elem "x" in
  let y = elem "y" in
  constr2 [x; y] <> constr2 [y; x] &&
  constr4 [x; y] <> constr4 [y; x]
);;

(*  Neutral left, do not eliminate if it is on the right
    However, ad hoc completion adds neutral right since it is
    an equational consequence of the assoc, neutral left, inverse left
 *)

(* testl 10 ( *)
(*   let x = elem "x" in *)
(*   let y = elem "y" in *)
(*   constr1 [x; y; neutral] <> constr1 [x; y] *)
(* );; *)

(* Not associative, do not flatten *)
testl 20 (
  let x = elem "x" in
  let y = elem "y" in
  let z = elem "z" in
  constr2 [x; constr2 [y; z]] <> constr2 [x; y; z]
);;

(* Idempotent, do not eliminate if not consecutive *)
testl 30 (
  let x = elem "x" in
  let y = elem "y" in
  (constr2 [x; y; x] <> constr2 [x; y]) &&
  (constr2 [x; y; x] <> constr2 [y; x])
);;

testl 40 (
  let x = elem "x" in
  let y = elem "y" in
  let z = elem "z" in
  (constr2 [x; y; z; x; y] <> constr2 [x; y; z]) &&
  (constr2 [x; y; z; x; y] <> constr2 [z; x; y]) &&
  (constr3 [x; y; z; x; y] <> constr3 [x; y; z]) &&
  (constr3 [x; y; z; x; y] <> constr3 [z; x; y])
);;

(* Idempotent right, do not eliminate if it is on the left *)
testl 50 (
  let x = elem "x" in
  let y = elem "y" in
  (constr2 [x; x; y] = constr2 [x; y]) &&
  (constr2 [x; y; x; y] = constr2 [x; y]) &&
  (constr3 [x; x; y] = constr3 [x; y]) &&
  (constr3 [x; y; x; y] = constr3 [x; y])
);;

(* Idempotent not associative *)
testl 60 (
  let x = elem "x" in
  let y = elem "y" in
  let z = elem "z" in
  let w = elem "w" in
  (constr2 [x; y; constr2 [y; z; w]] <> constr2 [x; constr2 [z; w]])
);;

(* Nilpotent, do not eliminate if not consecutive *)
testl 70 (
  let x = elem "x" in
  let y = elem "y" in
  (constr4 [x; y; x] <> constr4 [nil; y]) &&
  (constr4 [x; y; x] <> constr4 [y; nil]) &&
  (constr5 [x; y; x] <> constr5 [nil; y]) &&
  (constr5 [x; y; x] <> constr5 [y; nil])
);;

testl 80 (
  let x = elem "x" in
  let y = elem "y" in
  let z = elem "z" in
  (constr4 [x; y; z; x; y] <> constr4 [z; nil]) &&
  (constr4 [x; y; z; x; y] <> constr4 [nil; z]) &&
  (constr5 [x; y; z; x; y] <> constr5 [z; nil]) &&
  (constr5 [x; y; z; x; y] <> constr5 [nil; z])
);;

(* Nilpotent left, do not eliminate if it is on the right *)
testl 90 (
  let x = elem "x" in
  let y = elem "y" in
  (constr4 [y; x; x] = constr4 [y; nil]) &&
  (constr4 [x; y; x; y] = nil) &&
  (constr5 [y; x; x] = constr5 [y; nil]) &&
  (constr5 [x; y; x; y] = nil)
);;

(* (\* Nilpotent not associative *\) *)
testl 100 (
  let x = elem "x" in
  let y = elem "y" in
  let z = elem "z" in
  let w = elem "w" in
  (constr4 [x; y; constr4 [y; z; w]] <> constr4 [x; nil; constr4 [z; w]]) &&
  (constr4 [x; y; constr4 [y; z; w]] <> constr4 [x; constr4 [nil; z; w]])
);;

(* Inverse right, do not eliminate if it is on the left *)
(* ?? What is the meaning of side in inverse 
   x + (- x) : x is the inv of (-x) and conversely (-x) of x
*)
(*testl 110 (
  let x = elem "x" in
  let y = elem "y" in
  let z = elem "z" in
  (constr6 [y; inv x; x; z] <> constr6 [y; neutral; z])
);;
*)
(* Inverse, do not replace if not consecutive *)
testl 120 (
  let x = elem "x" in
  let y = elem "y" in
  let z = elem "z" in
  (constr1 [x; y; inv x; z] <> constr1 [neutral; y; z]) &&
  (constr1 [x; y; inv x; z] <> constr1 [y; neutral; z])
);;

(*
(* Inverse left, do not eliminate if it is on the right *)
testl 130 (
  let x = elem "x" in
  let y = elem "y" in
  let z = elem "z" in
  (constr1 [y; x; inv x; z] <> constr1 [y; neutral; z])
);;
*)
(* Inverse not associative *)
testl 140 (
  let x = elem "x" in
  let y = elem "y" in
  let z = elem "z" in
  let w = elem "w" in
  (constr6 [x; y; constr6 [inv y; z; w]] <> constr6 [x; neutral; constr6 [z; w]]) &&
  (constr6 [x; y; constr6 [inv y; z; w]] <> constr6 [x; constr6 [neutral; z; w]]) &&
  (constr6 [constr6 [x; y; z]; inv z; w] <> constr6 [constr6 [x; y; neutral]; w]) &&
  (constr6 [constr6 [x; y; z]; inv z; w] <> constr6 [constr6 [x; y]; neutral; w])
);;
