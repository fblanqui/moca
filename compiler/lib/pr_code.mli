(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2012,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: pr_code.mli,v 1.10 2012-06-04 13:01:22 weis Exp $ *)

(** {4 Printing the code we generate.} *)

val pr_structure : Format.formatter -> Code.structure -> unit;;
(** Prints a code structure in the indicated formatter. *)

val pr_expression : Format.formatter -> Code.exp -> unit;;
(** Prints an expression in the indicated formatter. *)

val pr_clause : Format.formatter -> Code.clause -> unit;;
(** Prints a clause in a matching construct. *)

val reset_line_count: unit -> unit;;
val incr_line_count: unit -> unit;;
val get_line_count: unit -> int;;
val set_output_filename: string -> unit;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
