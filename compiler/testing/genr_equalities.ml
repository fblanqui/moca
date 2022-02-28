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

(* $Id: genr_equalities.ml,v 1.39 2012-04-02 09:27:26 weis Exp $ *)

(** {Generation of equalities on values, deduced from moca relations.} *)

open Eqnrel
open Code
open Tgenr_base
open Typed_vars
open Genr_substitutions
open Symb
;;

let print_no_equalities_warning number exp =
  Format.eprintf
    "@[<v>@[<v 2>Warning: cannot test equation@ %a:@]@ \
     could not generate enough values for %n equations.@]@."
    Pr_code.pr_expression exp number
;;

let print_unknown_type_warning tname exp =
  Format.eprintf
    "@[<v>@[<v 2>Warning: cannot test equation@ %a:@]@ \
     could not assign ground types to type variables of type %s.@]@."
    Pr_code.pr_expression exp tname
;;

let print_unknown_generator_warning gname exp =
  Format.eprintf
    "@[<v>@[<v 2>Warning: cannot test equation@ %a:@]@ \
     unknown generator %s.@]@."
    Pr_code.pr_expression exp gname
;;

let print_unknown_generator_type_warning gname tname exp =
  Format.eprintf
    "@[<v>@[<v 2>Warning: cannot test equation@ %a:@]@ \
     unknown type constructor %s for generator %s.@]@."
    Pr_code.pr_expression exp tname gname
;;

let print_type_arity_mismatch_warning tname should_be is exp =
  Format.eprintf
    "@[<v>@[<v 2>Warning: cannot test equation@ %a:@]@ \
     arity mismatch for type %s (arity is %d, expected arity is %d).@]@."
    Pr_code.pr_expression exp tname is should_be
;;

let rec exp_subst_of_term subst term =
  match term with
  | Term.Var x -> subst x
  | Term.App (x, ts) ->
    let string = string_of_symbol x in
    match ts with
    | [t1; t2] when Pr_ocaml.is_infix string ->
      infix_name
        (exp_subst_of_term subst t1)
        string
        (exp_subst_of_term subst t2)
    | _ ->
      mk_construction string (List.map (exp_subst_of_term subst) ts)
;;

let exp_of_term =
  exp_subst_of_term (fun v -> make_Constant (Var.raw_string_of_var v))
;;

let exp_of_equation (left_term, right_term) =
  mk_equality
   (exp_of_term left_term)
   (exp_of_term right_term)
;;

let rec add_val_defs final_exp vars subst =
  match vars with
  | [] -> final_exp
  | (v, _) :: vs ->
    let v_ident = Longident.Lident (Var.raw_string_of_var v) in
    Code.let0 v_ident (subst v) (add_val_defs final_exp vs subst)
;;

let equations g rels =
(*  let gid = g.Parsetree.pgen_desc in *)
  if Check.is_listary_generator g then begin
(*    Format.eprintf "%a is a listary generator@."
      Pr_ocaml.pr_ident_in_prefix_position gid; *)
    Genr_listary_equations.testing_equations_of_rels g rels
  end else begin
(*    Format.eprintf "%a is a NOT a listary generator@."
      Pr_ocaml.pr_ident_in_prefix_position gid; *)
    List.map
      (function Equation.Mk (l, r) -> l, r)
      (Equation.EqnSet.elements (eqnset_of_rels g rels))
  end
;;

let genr_values_and_equalities_for_eqn number genr_values eqn =
  let exp = exp_of_equation eqn in
  try
    let vars = typed_vars_eqn eqn in
    let substs = genr_substs number genr_values vars in
    List.map (add_val_defs exp vars) substs
  with
  | Not_enough_values ->
    print_no_equalities_warning number exp;
    []
  | Unknown_type_name tname ->
    print_unknown_type_warning tname exp;
    []
  | Unknown_generator_type_name (gname, tname) ->
    print_unknown_generator_type_warning gname tname exp;
    []
  | Unknown_generator_name gname ->
    print_unknown_generator_warning gname exp;
    []
  | Type_arity_mismatch (tname, should_be, is) ->
    print_type_arity_mismatch_warning tname should_be is exp;
    []
;;

let genr_values_and_equalities number genr_values (name, _, rels, loc) =
  let g = mk_generator name loc in
  let eqns = equations g rels in
  let gen_values_and_eqs =
    genr_values_and_equalities_for_eqn number genr_values in
  Listutils.flat_map gen_values_and_eqs eqns
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
