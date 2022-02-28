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

(* $Id: genr_base.ml,v 1.91 2012-04-02 09:27:25 weis Exp $ *)

(* Base definitions for generation functions *)

open Parsetree
open Code
open Relation
open Genr_pattern
open Longident
;;

(** Generated function classification *)

type generated_function =
  | Construction of Parsetree.generator * Code.pattern list * Code.exp
  | Comparison of Code.lident * Code.pattern list * Code.exp
  | Construction_helper of Code.lident * Code.pattern list * Code.exp
  | Construction_record of Code.lident * Code.pattern list * Code.exp
;;

type generated_functions = generated_function list
;;

let is_construction_function f =
  match f with
    | Construction _ -> true
    | Construction_helper _
    | Construction_record _
    | Comparison _ -> false
;;

(* Do we use the order on relations as given in the file ? *)
let get_user_rel_order, set_user_rel_order =
  let rel_order = ref false in
  (fun () -> !rel_order),
  (fun () ->
   if Configuration.get_verbose () then
     Format.printf "User-order on@.";
   rel_order := true)
;;

let order_rels prec rels =
  if get_user_rel_order () then rels else sort_rels prec rels
;;

let order_relations prec relations =
  if get_user_rel_order () then relations else sort_relations prec relations
;;

type binary_fun_exp = exp -> exp -> exp
;;

(* Do we generate maximum sharing ? *)
let set_sharable_target, get_sharable_target =
  let sharable_target = ref false in
  (fun () -> sharable_target := true),
  (fun () -> !sharable_target)
;;

let get_eq_fun_def () =
  if get_sharable_target () then ["%eq"] else ["%equal"]
;;

(* equality function *)
let eq_function eq x y =
  if get_sharable_target () then infix_name x "==" y
  else eq x y
;;

(* Generate equality for type [tname] only if it is a potential quotient type *)
let genr_eq (tname, tdecl) =
   Pstr_primitive
     ("eq_" ^ tname,
      { pval_type = Otype.type_of_eq tname tdecl.ptype_params;
        pval_prim = get_eq_fun_def ();
      })
;;

let genr_projection (tname, tdecl) trep =
  Pstr_primitive
    ("from_" ^ tname,
     { pval_type = Otype.projection_type tname tdecl.ptype_params trep;
       pval_prim = ["%identity"];
     })
;;

(* Generation of a pattern for the LHS of a user's defined rule. *)
let genr_rule_pattern p =
  if get_sharable_target ()
  then Genr_pattern.add_underscores (remove_topconstr p)
  else remove_topconstr p
;;

(* Building the construction function name out of the generator name. *)
let construction_function_name = String.uncapitalize
;;

let prefix_string pref str = Printf.sprintf "%s%s" pref str
;;

let suffix_string suff str = prefix_string str suff
;;

let rec fix_ident fix_string fix = function
  | Lident ident -> Lident (fix_string fix ident)
  | Ldot (modid, ident) -> Ldot (modid, fix_string fix ident)
  | Lapply (modid1, modid2) -> Lapply (modid1, fix_ident fix_string fix modid2)
;;

let prefix_ident = fix_ident prefix_string;;

let fix_construction_function_ident fix_string =
  let fix_string fix gname = fix_string fix (construction_function_name gname) in
  fix_ident fix_string
;;

let prefix_construction_function_ident =
  fix_construction_function_ident prefix_string
and suffix_construction_function_ident =
  fix_construction_function_ident suffix_string
;;

let construction_function_ident =
  fix_construction_function_ident (fun _fix s -> s) ""
;;

(* Name generation functions from a generator name:
   [prefix_construction_function p g] is the name "prefix_name",
   where "prefix" is the contents of [p],
   and "name" is the lower case version of [g]. *)
let prefix_construction_function prefix g =
  prefix_construction_function_ident prefix g.Parsetree.pgen_desc
;;

(* Name generation functions from two generator names:
   [prefix_construction_function2 p g1 g2] is the name "prefix_name1_name2",
   where "prefix" is the contents of [p],
   and "name1" is the lower case version of [g1]
   and "name2" is the lower case version of [g2].
   For instance:
   [prefix_construction_function2 "insert"
      (Lident "Inv") (Lident "Mult")] gives
   "insert_inv_mult" and
   [prefix_construction_function2 "insert"
      (Lident "Mult") (Lident "Inv")] gives
   "insert_mult_inv".
*)
let prefix_construction_function2 prefix g1 g2 =
  prefix_construction_function
    (Otype.name_of_longident
       (prefix_construction_function prefix g1))
    g2
;;

let construction_function g = construction_function_ident g.pgen_desc
;;

let compare_function g = prefix_construction_function "compare_" g
;;

(* In case we use memoization for construction functions, this function
   generates the name of the memo table for the construction function
   associated to the generator [g]. *)
let construction_function_memo_table_ident g =
  suffix_construction_function_ident "_memo_table" g.pgen_desc
;;

(* Apply the construction function for generator [g]. *)
let construction g =
  let fid = construction_function_ident g.pgen_desc in
  function
  | [] -> Constant fid
  | [x] -> apply1 fid x
    (* The listary case must be treated first, since we should apply the
      construction function to a list, whatever the number of elements in
      the list of arguments given for the generator. *)
  | xs when Check.is_listary_generator g -> apply1 fid (List xs)
  | xs -> apply1 fid (Tuple xs)
;;

let construction0 g = construction g []
and construction1 g x = construction g [x]
and construction2 g x y = construction g [x; y]
;;

(* Various names for functions, built from the name of the type. *)

(* The name of the construction function that will share terms
   built with the head generator [g]. *)
let mk_sharing_function_ident g = prefix_ident "mk_" g.pgen_desc
;;

(* Direct construction of terms from generators:
   we bypass the construction function to directly apply a generator [g] to
   its arguments.
   In case of sharing, we still need to call a function : the sharing version
   of the generator. *)
let generator g args =
  let gid = g.pgen_desc in
  match args with
  | [] -> Constant gid
  | _ when get_sharable_target () ->
    Apply (Constant (mk_sharing_function_ident g), args)
  | [x] -> Generator (gid, x)
  | _ when Check.is_listary_generator g -> Generator (gid, List args)
  | _ -> Generator (gid, Tuple args)
;;

let generator0 g = generator g []
and generator1 g x = generator g [x]
and generator2 g x y = generator g [x; y]
;;

(* Generation of usual list operations. *)

let cons x y = infix_apply x (Lident "::") y
and append x y = infix_apply x (Lident "@") y
and nil = make_Constant "[]"
;;

let singleton x = cons x nil;;

(* Generator pattern construction. *)

let pattern_info g info args =
  let gid = g.pgen_desc in
  match args with
  | [] -> Constant gid
  | _ when get_sharable_target () -> Generator (gid, Tuple (info :: args))
  | [x] -> Generator (gid, x)
  | _ -> Generator (gid, Tuple args)
;;

let pattern g = pattern_info g underscore;;

let pattern0 g = pattern g []
and pattern1 g x = pattern g [x]
and pattern2 g x y = pattern g [x; y]
;;

(*let generator_info_underscore g = pattern_info underscore g;;
(* In case of sharing, we add an underscore to match the additional [info] field
   in the list of pattern arguments in the application of generator [g]. *)

let genr_pattern_info_underscore g root n =
  generator_info_underscore g (genr_args root n)
;;

let genr_pattern_info_underscores g info n =
  assert (n >= 0);
  let rec genr_underscore_args = function
    | 0 -> []
    | n -> underscore :: genr_underscore_args (n - 1) in
  pattern_info info g (genr_underscore_args n)
;;
*)
(* Functions to select a clause according to the side argument. *)

let clauses_of_side side left right clauses =
  match side with
  | Left -> left :: clauses
  | Right -> right :: clauses
  | Both -> left :: right :: clauses
;;

let clauses_left_or_right side left right clauses =
  match side with
  | Left -> left :: clauses
  | Right -> right :: clauses
  | Both -> assert false
;;

let clauses_both_as_left side left right clauses =
  match side with
  | Left
  | Both -> left :: clauses
  | Right -> right :: clauses
;;

(* Functions to generate clauses on demand. *)
let opt b c cls = if b then c :: cls else cls
and lazy_opt b c cls = if b then c () :: cls else cls
;;

let opts cond cls1 cls2 = if cond then cls1 @ cls2 else cls2;;

(** Comparison functions. *)

type comparisons = binary_fun_exp * binary_fun_exp * binary_fun_exp
;;

let compare_ident = Lident "compare"
;;

let comparison_function = function
  | None -> Constant compare_ident
  | Some lid -> Constant lid
;;

(* Comparison expressions. *)

(* The standard encodings of comparisons in terms of
   expression compare x y < (or = or >) 0. *)
let standard_comparisons lid =

  let standard_comp inf lid x y =
    infix_name (apply2 lid x y) inf (make_Constant "0") in

  let standard_lt = standard_comp "<"
  and standard_eq = standard_comp "="
  and standard_gt = standard_comp ">" in

  let eq_predicate =
    if get_sharable_target () then apply2 (Lident "==") else
    standard_eq lid in

  standard_lt lid, eq_predicate, standard_gt lid
;;

let pervasives_compare_ident = Ldot (Lident "Pervasives", "compare");;

let pervasives_comparisons = standard_comparisons pervasives_compare_ident;;

(* Generation of a triple of comparison functions for a given
   commutative operator. *)
let genr_comparisons = function
  (* Commutative generator with no special comparison provided: use the
     standards comparisons. *)
  | None -> standard_comparisons compare_ident
  (* Commutative generator with a special comparison provided: use it! *)
  | Some lid -> standard_comparisons lid
;;

(** {6 Mapping according to the distributivity.} *)

let map_of_direction = function
  | Dist_Inverse -> List.rev_map
  | Dist_Direct -> List.map
;;

let imap_of_direction = function
  | Dist_Inverse -> Longident.Ldot (Lident "List", "rev_map")
  | Dist_Direct -> Longident.Ldot (Lident "List", "map")
;;

(** {6 Generating normalization/generation traces} *)
open Location;;

let set_comments, get_comments =
  let otrace_yes = ref false in
  (fun () -> otrace_yes := true),
  (fun () -> !otrace_yes)
;;

let set_dline_trace, get_dline_trace =
  let dtrace_yes = ref false in
  (fun () -> dtrace_yes := true),
  (fun () -> !dtrace_yes)
;;

let diese_line loc =
  let f1, l1, _c1 = Location.get_pos_info loc.loc_start
  and f2, _l2, _c2 = Location.get_pos_info loc.loc_end in
  assert (f1 = f2);
  l1, f1
;;

let sline_code line file code =
  if get_dline_trace ()
  then Sline_directive (line, file, code)
  else code
;;

let trace_clause line file = function
  | Clause (x, y) -> Clause (x, sline_code line file y)
  | Clause_when (x, w, y) -> Clause_when (x, w, sline_code line file y)
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
