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

(* $Id: genr_memo.mli,v 1.3 2011-05-16 15:36:59 weis Exp $ *)

(** {6 Maximum sharing handling.} *)
val set_memoize_target : unit -> unit
;;
val get_memoize_target : unit -> bool
(** Do we generate maximum sharing ? *)
;;

val get_memo_table_size : unit -> int
;;
val set_memo_table_size : int -> unit
;;

val print_memo_hash_table_module : Format.formatter -> unit
(** Output module declarations and helpers for memoized functions *)
;;

val genr_memo_functions :
  string -> Parsetree.core_type option
  -> Otype.constructor_definition
  -> Genr_base.generated_functions
  -> Genr_base.generated_functions
(** Memoize construction functions of a given constructor [cdef]
    among all generated functions for this very constructor.
*)
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
 End:
*)
