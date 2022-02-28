(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: tbool_sharing.ml,v 1.4 2007-09-05 07:44:47 weis Exp $ *)

(* Testing Booleans. *)

open Gentest;;

open Bool_sharing;;

testing "Bool_sharing";;

let bimply p q = bor (bnot p, q);;

let bequiv p q = band (bimply p q, bimply q p);;

let tauto t = t = btrue;;

let p = btrue and q = bfalse;;

testi 0 (not (tauto (bimply p q)));;
testi 1 (tauto (bimply p p));;
testi 2 (tauto (bimply q q));;
