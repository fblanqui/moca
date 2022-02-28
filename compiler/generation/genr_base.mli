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

(* $Id: genr_base.mli,v 1.69 2011-05-16 14:07:04 weis Exp $ *)

(** {3 Basic definitions for generation functions.} *)

open Code
open Parsetree
;;

(* {6 Generated function classification.} *)

type generated_function =
  | Construction of Parsetree.generator * Code.pattern list * Code.exp
  | Comparison of Code.lident * Code.pattern list * Code.exp
  | Construction_helper of Code.lident * Code.pattern list * Code.exp
  | Construction_record of Code.lident * Code.pattern list * Code.exp
;;

type generated_functions = generated_function list
;;

val is_construction_function: generated_function -> bool
;;

(* {6 Comparison functions.} *)

type binary_fun_exp = Code.exp -> exp -> exp
(** The type of a function that takes two expression arguments and returns an
    expression that represents the application of the function to these
    arguments. *)
;;

type comparisons = binary_fun_exp * binary_fun_exp * binary_fun_exp
(** The triple of a lesser, an equal, and a greater predicate. *)
;;

val comparison_function : Longident.t option -> exp
(** A constant representing the name of the comparison function corresponding
    to the indicated identifier. Returns the standard comparison function
    if no identifier is provided. *)
;;

(** {6 Comparison expressions.} *)

val genr_comparisons : lident option -> comparisons
(** Generation of a triple of comparison functions for a given commutative
    operator. *)
;;

val pervasives_comparisons : comparisons
(** Triple representing OCaml's Pervasives.compare functions *)
;;

val eq_function : binary_fun_exp -> Code.exp -> exp -> exp
(** Returns the function that generates the code to call the proper equality
    function to its two code arguments.
    The proper equality function is "eq" or the [binary_fun_exp] argument,
    according to whether we have or have not to generate maximally shared
    terms for the relational type at hand. *)
;;

(** Do we use the user order for code generation ? *)
val get_user_rel_order : unit -> bool
;;
val set_user_rel_order : unit -> unit
;;

val order_rels : (relation -> int) -> relation list -> relation list
;;
val order_relations : (relation -> int) -> relations -> relations
;;

(** {6 Maximum sharing handling.} *)
val set_sharable_target : unit -> unit
;;
val get_sharable_target : unit -> bool
(** Do we generate maximum sharing ? *)
;;


(** {6 Handling function names and identifiers.} *)

val compare_function : Parsetree.generator -> lident
(** Comparison function ident from its generator *)
;;

val construction_function_name : string -> string
(** Construction function name from its generator (string) name. *)
;;
val construction_function_ident : uident -> lident
(** Construction function ident from its generator ident. *)
;;
val construction_function : Parsetree.generator -> lident
(** Construction function ident from its generator. *)
;;
val construction_function_memo_table_ident : Parsetree.generator -> lident
(** Construction function memo table ident from its generator. *)
;;

val prefix_ident : string -> lident -> lident
(** Adds a prefix to an identifier.
  [prefix_ident p id] is the identifier [p_id] is [id] is not qualified,
  [Mod.p_id] if [id] is qualified by module [Mod]. *)
;;

val prefix_construction_function : string -> Parsetree.generator -> lident
(** Name of the generation functions for a given prefix name
   and a given generator:
   [prefix_generator_function p g] is the ident "prefix_name",
   where "prefix" is the contents of [p],
   and "name" is the uncapitalized version of [g]. *)
val prefix_construction_function2 : string -> generator -> generator -> lident
(** Name of the generation functions for a given prefix name
   and the combination of two generators:
   [prefix_generator_function2 p g1 g2] is the ident "prefix_name1_name2",
   where "prefix" is the contents of [p],
   and "name1", "name2" are the uncapitalized versions of [g1], [g2]. *)
;;

val mk_sharing_function_ident : generator -> Code.lident
(** Name of the sharing generation functions for a given generator name. *)
;;

(** {6 Applying construction functions of generators.} *)

val construction : generator -> exp list -> exp
(** The generic (and most general) way to apply a construction function for a
  generator.
  [construction] properly takes care of the generator arity, whatever
  this arity could be.

  The following specialized versions are just particular case of the generic
  [construction] function.
*)
;;

val construction0 : generator -> exp
;;
val construction1 : generator -> exp -> exp
;;
val construction2 : generator -> exp -> exp -> exp
(** The application of the construction function for a zeroary, unary or
  binary generator. *)
;;

(** {6 Applying generators.} *)

(** We use the generators as unconstrainted term builders.

  Presumably, the following functions are only used once to define the
  construction function of a generator: we need it to build the (last)
  default clause of the construction function (unless we have a proof that a
  direct call to the generator is safe, because we know that the resulting
  term will be in normal form). *)

val generator : generator -> exp list -> exp
(** Apply the generator term constructor for the generator: in case of sharing,
    we properly call the sharing version of the generator. *)
;;
val generator0 : generator -> exp
;;
val generator1 : generator -> exp -> exp
;;
val generator2 : generator -> exp -> exp -> exp
(** Apply the generator term constructor for the generator: special cases for
  zeroary, unary, and binary generators. *)
;;

(** {6 Generation of usual list operations.} *)

(** These functions generate code values. *)
val nil : exp
;;
val singleton : exp -> exp
;;
val cons : exp -> exp -> exp
;;
val append : exp -> exp -> exp
;;

(** {6 Pattern construction for generators and functions.} *)
(*
val generator_info_underscore : Parsetree.generator -> Code.pattern list -> Code.pattern
(** Add an underscore as first argument to the generator application pattern.
    Used in the module [Genr_sharing] to skip the info field of the generator. *)
;;

val genr_pattern_info_underscore : Parsetree.generator -> string -> int -> Code.pattern
(** [genr_pattern_info_underscore g s n]
    Adds an underscore as first argument to the generator application pattern
    [g (x1, x2, ..., xn)] giving [g (_, x1, x2, ..., xn)].
    Also generates the variable names [x1, ..., xn] from suffix [s] and
    integer number [n].
    Used in the module [Genr_sharing] to obtain the most general pattern for
    generator [g], properly skipping the info field of the generator. *)
;;
*)
val genr_rule_pattern : Parsetree.pattern -> Parsetree.pattern
(** Generation of the LHS of a pattern matching clause for a user's defined
    rule. *)
;;

val pattern_info : generator -> Code.pattern -> Code.pattern list -> Code.pattern
(** Similar to generator application for the pattern case. *)
;;

val pattern : generator -> Code.pattern list -> Code.pattern
;;
val pattern0 : generator -> Code.pattern
;;
val pattern1 : generator -> Code.pattern -> Code.pattern
;;
val pattern2 : generator -> Code.pattern -> Code.pattern -> Code.pattern
(** Similar to generator application for the pattern case. *)
;;

(** {6 Generation of functions for relational types.} *)

val genr_eq : (string * type_declaration) -> Code.structure_item
(** Generate an equality predicate primitive for the type name given:
    for type [t], we generate the primitive [eq_t], bound to ["%equal"] if the
    values of the type are not maximally shared, or bound to ["%eq"]
    otherwise. *)
;;

val genr_projection : (string * type_declaration) ->
  core_type -> Code.structure_item
(** Generate a projection primitive for the (abbreviation) type name given. *)
;;

(** {6 Functions to select a clause according to the side argument.} *)

val clauses_of_side :
  relation_side -> clause -> clause -> clauses -> clauses
;;

val clauses_left_or_right :
  relation_side -> clause -> clause -> clauses -> clauses
;;

val clauses_both_as_left :
  relation_side -> clause -> clause -> clauses -> clauses
;;

(** {6 Functions to generate clauses on demand.} *)
val opt : bool -> clause -> clauses -> clauses
(* Conditional pattern matching clause adder.
   [opt c cl cls] returns the list of clauses [cl :: cls] or simply [cls]
   according to the condition [c].

   For instance, [opt commutative cl cls] adds [cl] to [cls]
   if the condition [commutative] evaluates to [true], otherwise it simply
   ignore [cl] and returns [cls] unmodified.

   Note: the imbrication of successive calls to [opt] is similar to the
   imbrication of successive calls to [ ( :: ) ] when building ordinary
   lists.

   For instance, if we denote by [cons] the prefix version of [::],
   we add the elements [\[x1; x2; x3;\]] in front of [default_list],
   by writing:
   [cons x1 (cons x2 (cons x3 default_list))].
   Similarly, we (conditionally) add the clauses [\[cl1; cl2; cl3\]]
   in front of [default_clauses] by writing
   [opt c1 cl1 (opt c2 cl2 (opt c3 cl 3 default_clauses))]
*)
;;
val lazy_opt : bool -> (unit -> clause) -> clauses -> clauses
(* Lazy version of the conditional pattern matching clause adder.
   [lazy_opt c gen_cl cls] returns the list of clauses [gen_cl () :: cls] or
   simply [cls] according to the evaluation result of the condition [c]. *)
;;

val opts: bool -> clauses -> clauses -> clauses
 (* Lifting opt to list of clauses *)
 ;;

(** {6 Mapping according to the distributivity.} *)

val map_of_direction : distributivity_direction ->
  ('a -> 'b) -> 'a list -> 'b list
(** Returns the [map] like function from the predefined [List] module that is
  suitable for the [distributivity_direction] argument.
  This function is used to generate the code. *)
;;

val imap_of_direction : distributivity_direction -> Longident.t
(** Returns the qualified identificateur of the mapping function of the
  predefined [List] module that is suitable for the
  [distributivity_direction] argument.
  This identifier will be used in the generated code. *)
;;

(** {6 Tracing normalization for generated code} *)

val diese_line: Location.t -> int * string
(** Get the line and file where the located element appears *)
;;

val set_dline_trace : unit -> unit
;;
val get_dline_trace : unit -> bool
(** Do we add diese line directives to [.ml] files ? *)
;;

val set_comments : unit -> unit
;;
val get_comments : unit -> bool
(** Do we output normalization traces ? *)
;;

val trace_clause : int -> string -> Code.clause -> Code.clause
(** Returns the code outputing the diese_line directives
    # [int] [string]
*)
;;



(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
