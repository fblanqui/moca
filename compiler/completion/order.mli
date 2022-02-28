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

(** {4 Term ordering utilities } *)

(** Result type for comparison functions *)
type ord =
   | Greater
   | Equal
   | Smaller
   | Uncomparable
;;

type 'a cmp = 'a -> 'a -> ord;;

(** Partition a list of pairs into those whose lhs is greater, equal, etc.
than the rhs *)
val partition :
  'a cmp -> ('a * 'a) list ->
  ('a * 'a) list * ('a * 'a) list * ('a * 'a) list * ('a * 'a) list
;;

(* basic functions on ord *)

val int_of_ord : ord -> int;; (* undefined on Uncomparable *)

val opp : ord -> ord;;

val comp : ord cmp;;

val ge : ord -> bool;;
val le : ord -> bool;;

(** Lexicographic extension of an ordering *)
val lex : 'a cmp -> 'a list cmp;;

(* lexicographic path ordering *)

open Term;;
open Symb;;

exception IncompletePrecedence of symbol * symbol;;

exception IncompatibleStatus of symbol * symbol;;

val rpo_fail : Parsetree.rpo_status SymbolMap.t -> symbol cmp -> term cmp;;

(** Recursive Path Ordering *)
val rpo : Parsetree.rpo_status SymbolMap.t -> symbol cmp -> term cmp;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
