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

open Group
;;

open Gentest
;;

testing "Group"
;;

(* Tests generated automatically that once failed *)

testi 0
      (let x = gen 38 in
       let y = opp (gen 11) in
       let z = opp (gen 11) in
       mult (mult (x, y), z) = mult (x, mult (y, z)))
