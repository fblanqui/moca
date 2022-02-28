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

(* $Id: genr_expression.ml,v 1.19 2011-11-03 07:45:57 weis Exp $ *)

open Parsetree
;;

let lower_long_ident id =
  try
    let gi = Check.find_generator_ident_info id in
    if Check.is_local_info gi & Check.is_private_info gi
    then Genr_base.construction_function_ident id
    else id
  with Not_found -> id
;;

(* We need to define lower_map before to use it with different types for x *)
let lower_map lower_fun = List.map (fun (x,e) -> (x, lower_fun e))
;;

let rec lower_case e = { e with pexp_desc = lower e.pexp_desc }

and lower_opt = function
  | Some e -> Some (lower_case e)
  | None -> None

and lower = function
  | Pexp_parens e -> Pexp_parens (lower_case e)
  | Pexp_begend e -> Pexp_begend (lower_case e)
  | Pexp_ident lid -> Pexp_ident (lower_long_ident lid)
  | Pexp_constant _ as e -> e
  | Pexp_let (rec_flag, l, e) ->
    Pexp_let (rec_flag, lower_map lower_case l, lower_case e)
  | Pexp_function (label, eopt, l) ->
    Pexp_function (label, lower_opt eopt, lower_map lower_case l)
  | Pexp_apply (e, l) -> Pexp_apply (lower_case e, lower_map lower_case l)
  | Pexp_match (e, l) -> Pexp_match (lower_case e, lower_map lower_case l)
  | Pexp_try (e, l) -> Pexp_try (lower_case e, lower_map lower_case l)
  | Pexp_tuple l -> Pexp_tuple (List.map lower_case l)
  | Pexp_construct (lid, eopt, b) ->
    Pexp_construct (lower_long_ident lid, lower_opt eopt, b)
  | Pexp_variant (l, eopt) -> Pexp_variant (l, lower_opt eopt)
  | Pexp_record (l, eopt) ->
    Pexp_record (lower_map lower_case l, lower_opt eopt)
  | Pexp_field (e, lid) -> Pexp_field (lower_case e, lower_long_ident lid)
  | Pexp_setfield (e1, lid, e2) ->
    Pexp_setfield (lower_case e1, lower_long_ident lid, lower_case e2)
  | Pexp_array l -> Pexp_array (List.map lower_case l)
  | Pexp_ifthenelse (e1, e2, eopt) ->
    Pexp_ifthenelse (lower_case e1, lower_case e2, lower_opt eopt)
  | Pexp_sequence (e1, e2) -> Pexp_sequence (lower_case e1, lower_case e2)
  | Pexp_while (e1, e2) -> Pexp_while (lower_case e1, lower_case e2)
  | Pexp_for (s, e1, e2, df, e3) ->
    Pexp_for (s, lower_case e1, lower_case e2, df, lower_case e3)
  | Pexp_constraint (e, ct1, ct2) -> Pexp_constraint (lower_case e, ct1, ct2)
  | Pexp_when (e1, e2) -> Pexp_when (lower_case e1, lower_case e2)
  | Pexp_send (e, s) -> Pexp_send (lower_case e, s)
  | Pexp_new lid -> Pexp_new (lower_long_ident lid)
  | Pexp_setinstvar (s, e) -> Pexp_setinstvar (s, lower_case e)
  | Pexp_override l -> Pexp_override (lower_map lower_case l)
  | Pexp_letmodule (s, m, e) -> Pexp_letmodule (s, m, lower_case e)
  | Pexp_assert e -> Pexp_assert (lower_case e)
  | Pexp_assertfalse -> Pexp_assertfalse
  | Pexp_lazy e -> Pexp_lazy (lower_case e)
  | Pexp_poly (e, ct) -> Pexp_poly (lower_case e, ct)
  (* Should recurse into fields of the object... *)
  | Pexp_object _ ->  failwith "Not yet implemented"
  | Pexp_open (module_ident, e) -> Pexp_open (module_ident, lower_case e)
  (* Should define lower module_expr and recurse... *)
  | Pexp_pack (_mod_expr, _pack_type) -> failwith "Not yet implemented"
  | Pexp_newtype (t, e) -> Pexp_newtype (t, lower_case e)
;;

(* Generation of a pattern matching clause for a user's defined
   rewrite rule (p -> e). *)
let genr_rule p e =
  let p = Genr_base.genr_rule_pattern p in
  match e.pexp_desc with
  | Pexp_when (e1, e2) ->
    let e1 = lower_case e1 and e2 = lower_case e2 in
    Code.Clause_when
      (Code.Ocaml_pattern p, Code.Ocaml_expression e1,
       Code.Ocaml_expression e2)
  | _ ->
    let e = lower_case e in
    Code.Clause (Code.Ocaml_pattern p, Code.Ocaml_expression e)
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
