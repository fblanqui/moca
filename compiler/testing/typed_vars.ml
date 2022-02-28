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

(* $Id: typed_vars.ml,v 1.33 2012-04-02 09:27:26 weis Exp $ *)

(** {Deducing mapping typed variables to values Code.exp.} *)

open Term;;
open Otype;;
open Parsetree;;
open Symb;;
open Check;;

exception Unknown_type_name of string ;;
exception Type_arity_mismatch of string * int * int ;;
exception Unknown_generator_name of string ;;
exception Unknown_generator_type_name of string * string ;;

let eq_typed_var (v1, t1) (v2, t2) = v1 = v2 && eq_type t1 t2
;;

let generator_info cname =
  try find_generator_name_info (string_of_symbol cname) with
  | Not_found ->
    raise (Unknown_generator_name (string_of_symbol cname))
;;

let is_listary cname =
  match string_of_symbol cname with
  | "[]" | "::" -> false
  | _ -> is_listary_info (generator_info cname)
;;

(* Return the most general type of a list value constructor gname
   (hence gname corresponds to "[]" or "::"). *)
let known_general_type gname =
  let mk_core_type tdesc = {
    ptyp_loc = Location.none;
    ptyp_desc = tdesc;
  } in
  let list_ident = Longident.Lident "list" in
  let list_type arg = Ptyp_constr (list_ident, [mk_core_type arg]) in
  let tvar_a = Ptyp_var "a" in
  let type_args =
    match gname with
    | "[]" -> []
    | "::" -> List.map mk_core_type [tvar_a; list_type tvar_a]
    | _ -> assert false in
  ("list", ["a"], type_args)
;;

(* Given a generator g, general_type returns a triple,
   (type_name, parameters, types of generator arguments). *)
let general_type g =
  let gname = string_of_symbol g in
  match gname with
  | "[]" | "::" -> known_general_type gname
  | _ ->
    let g_info = generator_info g in
    let type_name = g_info.gi_type_name in
    try
      let params = (find_type_name_declaration type_name).ptype_params in
      (type_name, params, g_info.gi_arg_types) with
    | Not_found ->
      raise (Unknown_generator_type_name (gname, type_name))
;;

let rec unif_arg_types topt (tname, params, arg_types) =
  match topt with
  | None -> arg_types
  | Some core_type ->
    begin match core_type.ptyp_desc with
    | Ptyp_any | Ptyp_var _ | Ptyp_arrow _ | Ptyp_tuple _
    | Ptyp_object _ | Ptyp_class _ | Ptyp_alias _
    | Ptyp_variant _ | Ptyp_poly _ -> arg_types
    | Ptyp_constr (real_ident, real_args) ->
      let binding = build_binding real_ident real_args tname params in
      subst binding arg_types
    | Ptyp_parens t -> unif_arg_types (Some t) (tname, params, arg_types)
    | Ptyp_package _ -> failwith "Ptyp_package not yet implemented"
    end

and build_binding real_ident real_args tname params =
  let lparams = List.length params in
  let lreal_args = List.length real_args in
(*  Format.eprintf "lparams=%d, lreal_args=%d, real_ident = %a, type name = %s\n"
    lparams lreal_args
    Pr_ocaml.pr_ident_in_prefix_position real_ident
    tname;*)
  if real_ident <> Longident.Lident tname
  then raise (Unknown_type_name tname)
  else if lparams <> lreal_args
  then raise (Type_arity_mismatch (tname, lparams, lreal_args))
  else List.combine params real_args
;;

let checked_types real_type_opt g terms =
  let g_type = general_type g in
  let arg_types = unif_arg_types real_type_opt g_type in
  if is_listary g (*terms*) then
    let arg_type = Otype.argument_of_type_list arg_types in
    Listutils.repeat (List.length terms) arg_type
  else arg_types
;;

let rec checked_typed_vars topt = function
  | Var _ -> []
  | App (c, terms) ->
    let terms_types =
      List.combine terms (checked_types topt c terms) in
    let aux_typed_vars = function
      | Var v, t -> [ v, t ]
      | App (c, ts), t -> checked_typed_vars (Some t) (App (c, ts)) in
    Listutils.flat_map aux_typed_vars terms_types
;;

let typed_vars = checked_typed_vars None
;;

let typed_vars_eqn (l, r) =
  Listutils.unique
    eq_typed_var (typed_vars l @ typed_vars r)
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
