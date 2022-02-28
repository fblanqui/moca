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

(* $Id: genr_listary.mli,v 1.7 2012-01-31 09:12:58 bonichon Exp $ *)

(** {3 Construction functions for listary generators} *)

val genr_function : Otype.constructor_definition ->
  Genr_base.generated_functions
(** From a listary generator definition, generate a list of definitions
    that implements its construction function. *)
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
