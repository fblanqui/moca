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

(* $Id: genr_nary.mli,v 1.6 2011-04-18 18:14:01 bonichon Exp $ *)

(** {3 Construction functions for n-ary generators.} *)

val genr_function :
  Otype.constructor_definition -> Genr_base.generated_functions
(** We treat the case of n-ary generators.

    Let's [g] be such a generator; we generate a n-ary function (here n-ary
    means that the function has a n-tuple as unique argument) that normalizes
    terms that are built with [g] as their head constructor. *)
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
