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

(* $Id: tvariable.ml,v 1.4 2012-06-04 13:01:22 weis Exp $ *)

(* Testing simple sharing over basic types. *)

open Gentest;;

open Variable;;

testing "Variable";;

let v1 = var "x";;
let v2 = var "x";;

testi 0 (v1 = v2);;
testi 1 (v1 != v2);;
