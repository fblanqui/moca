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

open Peano_list
;;

open Gentest
;;

testing "Peano_list"
;;

(* Automatic tests that once failed *)
testi 0
(let x = succ zero in
 let y = plus [succ zero; plus [zero; zero; zero]; succ zero] in
 let z =
   plus [succ zero; plus [zero; zero; zero]; plus [zero; zero; zero]] in
 plus [plus [x; y]; z] = plus [x; y; z])
;;

testi 1
(let x = succ (plus [zero; zero; zero]) in
 let y = plus [succ zero; plus [zero; zero; zero]; succ zero] in
 let z = succ (succ zero) in
 plus [plus [x; y]; z] = plus [x; y; z])
;;
