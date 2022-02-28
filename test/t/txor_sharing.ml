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

(* $Id: txor_sharing.ml,v 1.4 2008-02-11 19:37:33 weis Exp $ *)

(* Testing booleans. *)

open Gentest;;

open Xor_sharing;;

testing "Xor_sharing";;

let bor x y = y;;

let bnot x = x;;
