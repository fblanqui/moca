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

(** {4 Rewrite rules definitions } *)

(** The variables of the rhs must be included in the lhs *)

open Term;;
open Useful;;

type rule = private Mk of term * term;;

val mk : term * term -> rule;;

(** Comparison and equality up to lhs-variables renaming *)
val compare : rule -> rule -> int;;

val eq : rule -> rule -> bool;;

(** Print an equation *)
val fprintf : rule fprintf;;

(* useful modules *)

module OrdRul : ORD_PRT with type t = rule;;

module RulSet : Myset.S with type elt = rule;;

open Symb;;

(** Symbols of a set of rules *)
val symbols_of_rule : rule -> SymbolSet.t;;

val symbols_of_rules : RulSet.t -> SymbolSet.t;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
