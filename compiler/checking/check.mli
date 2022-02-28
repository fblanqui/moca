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

(* $Id: check.mli,v 1.50 2012-06-04 13:01:22 weis Exp $ *)

(** {4 Builds various hash tables for generators and types } *)

open Parsetree
;;

type locality =
   | Local
   | Global of string
(** The locality flag for a generator: is the generator defined in the local
    module or globally ?
*)
;;

type generator_arity =
   | Ga_zeroary
   | Ga_unary
   | Ga_binary
   | Ga_listary
   | Ga_mary of int
(** The generator associated to a constructor could have one of the following
    arity class:

 - [Ga_zeroary], if the generator is associated to a constant or 0-ary constructor,
 - [Ga_unary], if the generator is associated to a unary functional constructor,
 - [Ga_binary], if the generator is associated to a binary functional constructor,
 - [Ga_listary], if the generator is associated to a functional constructor
   with variable arity (i.e. a constructor that has a single argument which
   is a list of elements of the relational type at hand),
 - [Ga_mary n], if the generator is associated to a constructor with an
   arbitrary number of arguments [n].
   Note that for a generator with arity class [Ga_mary n]:
   - [n] can be [1] when the generator cannot be considered as having a
     variable arity,
   - [n] cannot be 0 or 2,
   - [n] can be greater than 3.
*)
;;

type generator_info = {
  gi_type_name : string;
  gi_type_params : string list;
  gi_target_type : core_type;
  gi_generator_name : string;
  gi_locality : locality;
  gi_arity : generator_arity;
  gi_arg_types : core_type list;
  gi_relations : relations;
  gi_location : Location.t;
  gi_priv_flag : Asttypes.private_flag;
}
(** The complete information associated to a generator. *)
;;

(** Possible errors. *)
type msg = string

type error =
   | Unknown_generator of Longident.t
   | No_neutral_element of Longident.t
   | Public_type_with_relations of string
   | Cannot_find_module of Longident.t
   | Not_supported of string
   | Relation_already_declared of Location.t
   | Incompatible_types of Longident.t * Longident.t
   | Invalid_pattern
   | Pattern_not_headed_by_correct_generator of Longident.t
   | Illegal_relation of Longident.t
   | Incompatible_relations of
       Longident.t * Parsetree.relation * Parsetree.relation
   | Unsound_ordering of msg
;;

exception Error of Location.t * error
;;

val raise_error : Location.t -> error -> 'a
;;

val report_error : Format.formatter -> error -> unit
;;

(** Various access/test functions. *)

val is_local_info : generator_info -> bool
;;
val is_private_info : generator_info -> bool
;;
val is_listary_info : generator_info -> bool
;;
val arity_of_info : generator_info -> int
;;

(** Enters a list of type definitions in the table. *)
val add_type_decls : locality -> (string * type_declaration) list -> unit
;;

(** Access functions to generators and relational types, as recorded into the
    global Moca tables.

 The find kind access functions may raise Not_found. *)

(** Access to generators. *)
val is_generator_ident : Longident.t -> bool
;;
val find_generator_ident_info : Longident.t -> generator_info
;;
val find_generator_name_info : string -> generator_info
;;

(** Access to types. *)
val is_type_ident : Longident.t -> bool
;;
val find_type_ident_declaration : Longident.t -> type_declaration
;;
val find_type_name_declaration : string -> type_declaration
;;

(** Various access/test functions that may raise Unknown_generator. *)
val find_generator_info : Parsetree.generator -> generator_info
;;

val is_listary_generator : Parsetree.generator -> bool
;;
val is_local_generator : Parsetree.generator -> bool
;;
val is_private_generator : Parsetree.generator -> bool
;;
val arity_of_generator : Parsetree.generator -> int
;;
val get_arity_of_generator : Parsetree.generator -> generator_arity
;;
val is_truly_unary_generator : Parsetree.generator -> bool
;;

val neutral_element_of_generator : Parsetree.generator -> Parsetree.generator
(** Gives the neutral element of [g] or else raise No_neutral_element *)
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
