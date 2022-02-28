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

(* $Id: tbool_list.ml,v 1.4 2012-06-04 13:01:22 weis Exp $ *)

(* Testing Booleans. *)

open Gentest;;

open Bool_list;;

testing "Bool_list";;

let bimply p q = bor [bnot p; q];;

let bequiv p q = band [bimply p q; bimply q p];;

let tauto t = t = btrue;;

let p = btrue and q = bfalse and r = btrue;;

testi 0 (not (tauto (bimply p q)));;
testi 1 (tauto (bimply p p));;
testi 2 (tauto (bimply q q));;
testi 3 (tauto (bimply q q));;
testi 4
  (tauto (bimply (bimply p (bimply q r)) (bimply (bimply p q) r)));;
