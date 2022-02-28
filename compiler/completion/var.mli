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

(** {4 Variable definition and utilities for completion purposes } *)

type var = int;;

module VarSet : Set.S with type elt = var;;
module VarMap : Map.S with type key = var;;

(** Return a fresh variable if called with a string for the first time.
    Return this variable if called again with the same string. *)
val var_of_string : string -> var;;

(** Return a fresh variable not bound to a string by var_of_string. *)
val fresh_var : unit -> var;;

(** Remove all string-variable bindings *)
val clear_vars : unit -> unit;;

(** Convert n>=0 into xn and n<0 into yn *)
val raw_string_of_var : var -> string;;

(** Inverse of var_of_string. create a fresh string if necessary. *)
val string_of_var : var -> string;;

val pr_var : Format.formatter -> var -> unit
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
