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

(* $Id: relation.mli,v 1.13 2012-06-04 13:01:22 weis Exp $ *)

(** {4 Operations on relations of relational types.} *)

open Parsetree;;

(** {1 Test functions on relations.} *)

val is_rewrite : relation -> bool;;
val is_not_rewrite : relation -> bool;;
val is_commutative : relation -> bool;;
val is_associative : relation -> bool;;
val is_involutive : relation -> bool;;
val is_absorbing : relation -> bool;;
val is_neutral : relation -> bool;;
val is_idempotent : relation -> bool;;
val is_inverse : relation -> bool;;
val is_distributive : relation -> bool;;
val is_nilpotent : relation -> bool;;

(** {1 Test functions on lists of relations.} *)

val has_commutative : relations -> bool;;
val has_associative : relations -> bool;;
val has_idempotent : relations -> bool;;
val has_absorbing : relations -> bool;;
val has_absorbent : relations -> bool;;
val has_neutral : relations -> bool;;
val has_nilpotent : relations -> bool;;
val has_idempotent_and_nilpotent : relations -> bool;;

(** {1 Access function on lists of relations.} *)
val get_relation_list : relations  -> relation list;;

val find_relation : (relation -> bool) -> relation list -> relation;;

(** {1 Selection functions on lists of relations.} *)

val rewrite : relations -> (pattern * expression) list;;
val commutative : relations -> Longident.t option;;
val associative : relations -> relation_side option;;
val absorbent : relations -> (relation_side * generator) option;;
val absorbing : relations -> (relation_side * generator) option;;
val neutral : relations -> (relation_side * generator) option;;
(** Returns the first neutral element (Left, Right, or Both)
    for the generator. *)
val neutral_left : relations -> (relation_side * generator) option;;
(** Returns a left neutral element for the generator if any. *)
val neutral_right : relations -> (relation_side * generator) option;;
(** Returns a right neutral element for the generator if any. *)
val neutral_both : relations -> (relation_side * generator) option;;
(** Returns the neutral element (simultaneously Left and Right, or Both)
    for the generator. *)
val nilpotent : relations -> (relation_side * generator) option;;
val idempotent : relations -> relation_side option;;
val inverse : relations ->
  (relation_side * generator * generator option) option
;;
val distributive : relations ->
  (relation_side * generator * generator option * distributivity_direction)
    list
;;

(** {1 Test functions on generators and lists of relations.} *)

val is_neutral_generator : generator -> relations -> bool;;

(** {1 Comparison functions.} *)

val precedence : relation -> int;;

val compare_side : relation_side -> relation_side -> int;;
val eq_side : relation_side -> relation_side -> bool;;

val compare_generator : generator -> generator -> int;;
val eq_generator : generator -> generator -> bool;;
(** Compare generators. *)

val compare_relation : (relation -> int) -> relation -> relation -> int;;
(** Comparison function on relations based on precedence.
    Note that two rewrite rules, two structure items,
    two commutative, or two associative relations are always considered
    equal. *)

val sort_rels : (relation -> int) -> relation list -> relation list;;
val sort_relations : (relation -> int) -> relations -> relations;;

module RelOrd : Set.OrderedType with type t = relation;;
module RelMap : Map.S with type key = relation;;
module RelSet : Myset.S with type elt = relation;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
