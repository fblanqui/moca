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

open Group_1_list
;;

open Gentest
;;

testing "Group_1_list"
;;

(* Tests generated automatically that once failed *)

testi 0
(let x = opp (opp one) in
 let y = opp (opp zero) in
 let z = add [opp zero; opp one; zero] in
 let w = add [opp one; opp zero; opp one] in
 let x1 = add [opp one; add [one; one; one]; add [one; zero; one]] in
 add [x; add [y; z]; add [w; x1]] = add [x; y; z; w; x1])
;;

testi 1 (let x = opp (add [one; zero; zero]) in
         add [x; zero; zero] = x)
;;
