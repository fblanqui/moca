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

(* $Id: pr_ocaml.mli,v 1.18 2012-06-04 13:01:22 weis Exp $ *)

(** {4 Printing Objective Caml types and expressions.} *)

open Format;;
open Parsetree;;

val pr_constant : formatter -> Asttypes.constant -> unit
;;

val string_of_constant : Asttypes.constant -> string
;;

val pr_structure_sep : formatter -> unit
;;

val is_infix : string -> bool
;;

val is_infix_longident : Longident.t -> bool
;;

val pr_ident_in_infix_position : formatter -> Longident.t -> unit
;;

val pr_ident_name_in_prefix_position : formatter -> string -> unit
;;

val pr_ident_in_prefix_position : formatter -> Longident.t -> unit
;;

val pr_core_type : formatter -> core_type -> unit
;;

val pr_value_description : formatter -> value_description -> unit
;;

val pr_type_definitions :
  formatter -> (string * type_declaration) list -> unit
;;

val pr_type : formatter -> string * string list -> unit
;;

val pr_relation : formatter -> relation -> unit
;;

val pr_params : formatter -> string list -> unit
;;

val pr_expression : formatter -> expression -> unit
;;

val pr_pattern : formatter -> pattern -> unit
;;

val pr_verbatim_structure_item : formatter -> structure_item -> unit
;;

val pr_verbatim_structure_items : formatter -> structure_item list -> unit
;;

val pr_structure_item_desc : formatter -> Parsetree.structure_item_desc -> unit
;;

(* Auxiliaries for printing several values *)

val pr_suffix_vals_fmt : (unit, formatter, unit) format ->
  (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
;;

val pr_sep_vals_fmt : (unit, formatter, unit) format ->
  (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
;;

val pr_sep_vals : string ->
  (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
