(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Laura Lowenthal, projet Protheo, INRIA Lorraine           *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: typed_vars.mli,v 1.13 2011-11-03 18:35:57 weis Exp $ *)

(** {Deducing mapping of typed variables to values of type Code.exp.} *)

exception Unknown_type_name of string
;;
exception Type_arity_mismatch of string * int * int
;;
exception Unknown_generator_name of string
;;
exception Unknown_generator_type_name of string * string
;;

val typed_vars_eqn : Term.term * Term.term -> (Var.var * Parsetree.core_type) list
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
