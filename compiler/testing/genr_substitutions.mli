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

(* $Id: genr_substitutions.mli,v 1.8 2012-06-04 13:36:52 weis Exp $ *)

(** {Generation of substitutions mapping typed variables to values Code.exp.} *)

exception Not_enough_values
;;

val genr_substs :
  int -> (Parsetree.core_type -> int -> Code.exp list)
  -> (Var.var * Parsetree.core_type) list -> (Var.var -> Code.exp) list
(** Generates a list of n substitutions, i.e. functions mapping each variable to a value of its corresponding type. Uses a function that generates a specified number of values of a given type. Raises Not_enough_values if it does not get enough values to generate all the required substitutions. *)
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
