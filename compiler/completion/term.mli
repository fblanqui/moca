(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(** {4 Term data structure }*)

open Var;;
open Symb;;

type term =
  | Var of var
  | App of symbol * term list;;

val vars : term -> VarSet.t;;

val vars_of_terms : term list -> VarSet.t;;

val list_vars : term -> var list;;

val list_vars_of_terms : term list -> var list;;

val is_linear : term -> bool;;

val symbols : term -> SymbolSet.t;;

val symbols_of_terms : term list -> SymbolSet.t;;

val occurs : var -> term -> bool;;

val fprintf : Format.formatter -> term -> unit;;

val rename : var VarMap.t -> term -> term;;

(** Comparison of terms modulo variable renaming *)
val compare : term -> term -> int;;

val eq : term -> term -> bool;;

(** Linearize a term *)
val linearize : term -> term * var VarMap.t;;

(** Pretty print. *)
val pr_term : Format.formatter -> term -> unit
;;


(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
