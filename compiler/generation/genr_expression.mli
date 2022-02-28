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

(* $Id: genr_expression.mli,v 1.7 2010-09-20 17:48:06 bonichon Exp $ *)

(** {3 Generation of expressions for construction functions.} *)

open Parsetree
;;

val lower_case : expression -> expression
(** Replaces constructor calls by construction function calls in an
    expression. *)
;;

val genr_rule : Parsetree.pattern -> Parsetree.expression -> Code.clause
(** Generation of a pattern maching clause for a user's defined rewrite rule
    [Rule (p, e)], meaning that pattern [p] should be rewritten
    as expression [e]. *)
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
