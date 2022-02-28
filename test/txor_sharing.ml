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

(* $Id: txor_sharing.ml,v 1.5 2012-04-02 20:25:09 weis Exp $ *)

(* Testing booleans. *)

open Gentest;;

open Xor_sharing;;

testing "Xor_sharing";;

testl 0 (btrue <> bfalse);;
testl 01 (btrue <> bvar "x");;
