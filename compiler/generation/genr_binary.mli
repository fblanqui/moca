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

(* $Id: genr_binary.mli,v 1.14 2012-01-31 09:12:58 bonichon Exp $ *)

(** {3 Construction functions for binary generators.} *)

val genr_function :
  Otype.constructor_definition -> Genr_base.generated_functions
(** [genr_function g] generates a binary function (here binary
    means that the function has a pair as unique argument) that normalizes
    terms that are built with [g] as their head constructor.

    This is only valid for binary generators.
*)
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
