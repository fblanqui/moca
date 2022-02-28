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

(* $Id: atom.mli,v 1.2 2009-04-22 09:49:32 weis Exp $ *)

type atom = private int
;;

external from : atom -> int = "%identity"
;;

val make : string -> atom
;;

val name : atom -> string
;;

val compare : atom -> atom -> int
;;

external eq : atom -> atom -> bool = "%eq"
;;
