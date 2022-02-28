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

(* $Id: genr.mli,v 1.13 2010-09-20 17:48:06 bonichon Exp $ *)

(** {3 Generation of construction functions for a type definition.} *)

val genr_functions :
  (string * Parsetree.type_declaration) list -> Code.structure_item list
;;
(** The global generation procedure: it generates the whole set of construction
  function definitions from a given type definition. *)
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
