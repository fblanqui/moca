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

(* $Id: code.mli,v 1.18 2012-06-04 13:01:22 weis Exp $ *)

(** {4 Definitions and library for the emitted AST } *)

type lident = Longident.t;;
(** Lowercase ident. *)
type uident = Longident.t;;
(** Uppercase ident. *)
type iident = Longident.t;;
(** Infix ident. *)
type ident = Longident.t;;
(** Any kind of ident. *)

type iname = string;;
(** The name of an infix symbol. *)
type tname = string;;
(** The name of a type declared in the AST. *)
type filename = string;;

type exp =
   | Constant of ident
   | Var of lident
   | Match of exp * clauses
   | Try of exp * clauses
   | Generator of uident * exp
   | Apply of exp * exp list
   | Infix of exp * iident * exp
   | If of exp * exp * exp
   | Let of lident * pattern list * exp * exp
   | Letrec of lident * pattern list * exp * exp
   | Sequence of exp * exp * exp list
   | Record of (lident * exp) list
   | Tuple of exp list
   | List of exp list
   | Ocaml_pattern of Parsetree.pattern
   | Ocaml_expression of Parsetree.expression
   | Function of clauses
   | Sline_directive of int * filename * exp
   | Comment of string * exp

and pattern = exp

and clause =
  | Clause of pattern * exp
  | Clause_when of pattern * exp * exp

and clauses = clause list
;;

type structure_item =
   (** Evaluates an expression *)
   | Pstr_eval of exp
   (** Defines a set of possibly recursive functional values *)
   | Pstr_value of Asttypes.rec_flag * (lident * exp list * exp) list
   (** Defines one primitive *)
   | Pstr_primitive of string * Parsetree.value_description
   (** Defines one type *)
   | Pstr_type of (string * Parsetree.type_declaration) list
   (** Defines one exception *)
   | Pstr_exception of string * Parsetree.exception_declaration
   (** Opens a module *)
   | Pstr_open of Longident.t
   (** Includes a module *)
   | Pstr_include of Parsetree.module_expr
   (** Comment belonging to a structure item *)
   | Pstr_comment of string * structure_item
;;

type structure = structure_item list
;;

(* Replace in a pattern all variables by underscore. *)
val underscored_ocaml_pattern : Parsetree.pattern -> Parsetree.pattern;;
val underscored_pattern : exp -> exp;;

(* Replace in a non-when clause lhs pattern all variables by underscore. *)
val underscored_clause : clause -> clause;;

val prefix_longident : string -> Longident.t -> Longident.t;;
(** To add a prefix string to a qualified (long) identifier. *)

(** To define a toplevel let rec definition in a module implementation. *)
val topletrec : (lident * pattern list * exp) list -> structure_item;;

(** Globally predefined identifiers. *)

val underscore : pattern;;

val x : exp;;
val y : exp;;
val z : exp;;
val t : exp;;
val u : exp;;

val xs : exp;;
(** When binding a list of arguments. *)

val genr_args : string -> int -> pattern list;;
(** [genr_args root n] returns the list of patterns [root1; ...; rootn]. *)

val genr_numbered_args : string -> int -> (int * exp) list;;
(** [genr_numbered_args root n] returns [1, root1; ...; n, rootn]. *)

val genr_underscores : int -> pattern list;;
(** [genr_underscores n] returns a list of [n] [_] patterns. *)

val make_Constant : string -> exp;;
(** Generate a constant expression from the given [string]. *)

val make_Var : string -> exp;;
(** Generate a variable expression from the name of the given ident. *)

(** {1 Application macros } *)

val apply : lident -> exp list -> exp;;
val apply1 : lident -> exp -> exp;;
val apply2 : lident -> exp -> exp -> exp;;
val apply3 : lident -> exp -> exp -> exp -> exp;;
(** Apply an ident to its argument(s). *)

val infix_apply : exp -> iident -> exp -> exp;;
(** Apply an infix ident to its arguments. *)

val infix_name : exp -> iname -> exp -> exp;;
(** Apply a given infix name to its arguments. *)

(** {1 Other useful auxiliaries } *)

val let0 : lident -> exp -> exp -> exp;;
val let1 : lident -> pattern -> exp -> exp -> exp;;
val pair : exp -> exp -> exp;;
val raise_error : string -> exp;;
val division_by_absorbent : exp;;
val failure_division_by_absorbent : exp;;
val failwith_division_by_absorbent : exp;;
val sequence : exp -> exp -> exp;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
