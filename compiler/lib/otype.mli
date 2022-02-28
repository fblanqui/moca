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

(* $Id: otype.mli,v 1.35 2012-06-04 13:01:22 weis Exp $ *)

(** {4 Functions on OCaml types.} *)

open Parsetree
;;

val opposite : relation_side -> relation_side
;;

(** {1 Converting longident and generator into string } *)

val name_of_longident : Longident.t -> string
val name_of_generator : generator -> string
val qualified_name_of_longident : Longident.t -> string
val qualified_name_of_generator : generator -> string
;;

(** Functions building relations *)

val mk_rel : rel_desc -> relation
;;

(** {1 Functions building core types } *)

val mk_type : core_type_desc -> core_type
;;
val var_type : string -> core_type
val constr_type : string -> core_type list -> core_type
val poly_type : string -> string list -> core_type
val constant_type : string -> core_type
val bool_type : core_type
val arrow_type : core_type -> core_type -> core_type
val tuple_type : core_type list -> core_type
val list_type : core_type -> core_type
val type_of_eq : string -> string list -> core_type
(* The types of the equality function for the type name argument. *)
val projection_type : string -> string list -> core_type -> core_type
val injection_type : string -> string list -> core_type -> core_type
(* The types of the injection and projection functions
    for type abbreviations. *)
;;

(** Functions testing/accessing core types. *)

val is_type_list : core_type -> bool
(* Checks if the type argument is indeed the [Pervasives.list] list
    functional type constructor. *)
;;

val argument_of_type_list : core_type list -> core_type
(** Returns the type argument of the Caml compiler list type
    functional type constructor.
    Raises Invalid_argument if the argument is not the [Pervasives.list] type
    constructor or the list of type arguments has not exactly on type. *)
;;

(** Syntactic equality ignoring locations *)
val eq_type : core_type -> core_type -> bool
;;

(** Comparison on core types (ignore locations). *)
val compare_core_type : core_type -> core_type -> int
;;

(** Given an association mapping variable names to core types,
    applies it to the given core type list as a substitution *)
val subst : (string * core_type) list -> core_type list -> core_type list
;;

(** {1 Functions on type declarations. } *)

val add_rules : relation list -> relations -> relations
(* Add relations. *)
;;

val remove_rules : relations -> relations
(* Remove rewrite relations. *)
val remove_rules_in_type_declaration : type_declaration -> type_declaration
(* Remove rewrite relations. *)
;;

val comment_relations_in_type_declaration :
  type_declaration -> type_declaration
(* To encapsulate relations in a [Comment] annotation. *)
;;

val make_public_type_declaration : type_declaration -> type_declaration
(* Take a possibly private type declaration and turns it into a public type
    declaration.  *)
;;

val get_structure_items_of_type_declaration :
  type_declaration -> structure_item list
  (* Extracts the value definitions which are private to a type
     definition. *)
;;

val is_variant_type_declaration : type_declaration -> bool
  (* Checks if a type declaration is a variant type declaration. *)
val is_private_type_declaration : type_declaration -> bool
(* Checks if a type declaration is private or not. *)
val is_private_variant_type_declaration : type_declaration -> bool
(* Checks if a type declaration is private variant type declaration
    or not. *)
;;

(** {1 Functions on constructor definitions. } *)

type constructor_definition
  = string * core_type list * relations * Location.t
;;

val generator_of_cdef : constructor_definition -> generator
val relations_of_cdef : constructor_definition -> relations
val arity_of_cdef : constructor_definition -> int
val has_relations : relations -> bool
val has_relations_cdef : constructor_definition -> bool
val has_relations_cdefs : constructor_definition list -> bool
;;

(** Simplify a list of relations: if a relation occurs both as Left
and Right, then it is replaced by Both. *)

val norm : relations -> relations
;;

(** {1 Functions building patterns } *)

val mk_pat : pattern_desc -> pattern
;;

(** {1 Functions building expressions } *)

val mk_exp : expression_desc -> expression
;;
val mk_when : expression -> expression -> expression
val mk_apply : expression -> expression list -> expression
val mk_ident : string -> expression
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
