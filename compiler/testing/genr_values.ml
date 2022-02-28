(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Laura Lowenthal, projet Protheo, INRIA Lorraine           *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: genr_values.ml,v 1.22 2012-04-02 08:37:48 weis Exp $ *)

(** {Generation of values of a given type} *)

open Parsetree
open Tgenr_base
open Otype
;;

let choose rand n vals =
  if rand
  then Randomutils.choose_many n vals
  else Listutils.take n vals
;;

(** Generates a list of up to n values of the specified type declaration *)
let rec genr_values rand n max_depth type_decl type_args =
  let param_binding = List.combine type_decl.ptype_params type_args in
  match type_decl.ptype_kind with
  | Ptype_variant cdefs ->
    let values_per_constr =
      List.map
        (genr_constructor_values rand n max_depth param_binding)
        cdefs in
    let resorted_vals =
      List.flatten (Listutils.transpose values_per_constr) in
    choose rand n resorted_vals
 | Ptype_abstract _ -> []
 | Ptype_record _ -> []

(** Generates a list of up to n values headed by the specified
    constructor definition, whose tree has a maximum depth (number of
    nested constructors) *)
and genr_constructor_values
      rand n max_depth param_binding constr_def =
  if max_depth <= 0 then [] else
  match constr_def with
  | constr_name, [], _, _ ->
      [mk_non_listary_construction constr_name []]
  | constr_name, args, _, _ ->
      let real_args = subst param_binding args in
      List.map (mk_non_listary_construction constr_name)
        (genr_lcore_values rand n (max_depth - 1) real_args)

(** Generates a list of up to n values of the specified core type,
    whose tree has a maximum depth (number of nested constructors) *)
and genr_core_values rand n max_depth core_type =
  if max_depth < 0 then [] else
  match core_type.ptyp_desc with
  | Ptyp_any -> []
      (* Replace all free variables with ints *)
  | Ptyp_var _ ->
      genr_constr_values rand n max_depth
        (Longident.Lident "int") []
  | Ptyp_arrow _ -> []
  | Ptyp_tuple inner_core_types ->
      List.map mk_tuple
        (genr_lcore_values rand n max_depth inner_core_types)
  | Ptyp_constr (ident, args) ->
      genr_constr_values rand n max_depth ident args
  | Ptyp_object _ -> []
  | Ptyp_class _ -> []
  | Ptyp_alias (inner_core_type, _) ->
      genr_core_values rand n max_depth inner_core_type
  | Ptyp_variant _ -> []
  | Ptyp_poly _ -> []
  | Ptyp_package _ -> failwith "Not yet implemented"
  | Ptyp_parens core_type ->
      genr_core_values rand n max_depth core_type

(** Generates a list of n values combining the specified core types *)
and genr_lcore_values rand n max_depth core_type_list =
  let value_lists =
    List.map (genr_core_values rand n max_depth) core_type_list in
  let combined_values = Listutils.combine_all value_lists in
  choose rand n combined_values

(** Generates a list of n values of the specified type constructor *)
(* genr_constr_values :
    randomize -> max_length -> nesting_depth ->
    Longident.t -> core_type list -> Code.exp list *)
and genr_constr_values rand n max_depth constr_ident args =
  match constr_ident with
  | Longident.Lident "int" -> genr_ints rand n
  | Longident.Lident "char" -> genr_chars rand n
  | Longident.Lident "string"-> genr_strings rand n
  | Longident.Lident "float" -> genr_floats rand n
  | Longident.Lident "int32" -> genr_int32s rand n
  | Longident.Lident "int64" -> genr_int64s rand n
  | Longident.Lident "nativeint" -> genr_nativeints rand n
  | Longident.Lident "unit" -> genr_units rand n
  | Longident.Lident "list" ->
    let list_size = 3 in
    let list_type = List.hd args in
    let types = Listutils.repeat list_size list_type in
    List.map mk_list (genr_lcore_values rand n max_depth types)
  | Longident.Lident _
  | Longident.Ldot _
  | Longident.Lapply _ as longident ->
    begin try
      let tname = Otype.name_of_longident longident in
      let type_decl = Check.find_type_name_declaration tname in
      genr_values rand n max_depth type_decl args with
    | Not_found -> [] end
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
