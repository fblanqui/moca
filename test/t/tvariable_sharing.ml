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

(* $Id: tvariable_sharing.ml,v 1.1 2007-09-05 07:44:14 weis Exp $ *)

(* Testing simple sharing over basic types. *)

open Gentest;;

open Variable_sharing;;

testing "Variable_sharing";;

let v1 = var "x";;
let v2 = var "x";;

testi 0 (v1 = v2);;
testi 1 (v1 == v2);;
