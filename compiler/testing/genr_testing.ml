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

(* $Id: genr_testing.ml,v 1.24 2012-04-02 09:27:26 weis Exp $ *)

(** {3 Generation of testing expressions for a moca type declaration} *)
open Parsetree
open Tgenr_base
;;

(* Preamble *)

let gentest_module_name = "Gentest"
let testing_function_name = "testing"
let test_function_name = "testi"
let gen_num = ref 0
;;

let testing_module module_name seed =
  let testing_function_ident =
    Longident.Lident testing_function_name in
  let testing_message =
    mk_string (Printf.sprintf "%s (automatic) - seed: %d" module_name seed) in
  Code.Pstr_eval (Code.apply1 testing_function_ident testing_message)
;;

let genr_test_preamble module_name seed = [
  Code.Pstr_open (Longident.Lident module_name);
  Code.Pstr_open (Longident.Lident gentest_module_name);
  testing_module module_name seed;
]
;;

(* We want to know if there is an absorbent relation somewhere in the
   constructor definitions.
   If there is none, we avoid to generation spurious
   try ... with Division_by_absorbent in test cases.
   This is a crude first attempt at this reduction that should however be nicer
   in the end.
*)
let need_division_by_absorbent cdefs =
  let rec has_absorbent_cdefs = function
    | [] -> false
    | cdef :: cdefs ->
      Relation.has_absorbent (Otype.relations_of_cdef cdef)
      || has_absorbent_cdefs cdefs in
  has_absorbent_cdefs cdefs
;;

let genr_testing rnd num_tests val_depth (_, td) =
  match td.ptype_kind, td.ptype_private with
  (* We only generate tests for private sum types. *)
  | Ptype_record _, Asttypes.Public
  | Ptype_abstract _, Asttypes.Public
  | Ptype_variant _, Asttypes.Public
  | Ptype_record _, Asttypes.Private
  | Ptype_abstract _, Asttypes.Private -> false, []
  | Ptype_variant cdefs, Asttypes.Private ->
    let need_division_by_absorbent = need_division_by_absorbent cdefs in
    let genr_values ct n = Genr_values.genr_core_values rnd n val_depth ct in
    need_division_by_absorbent,
    Listutils.flat_map
      (Genr_equalities.genr_values_and_equalities num_tests genr_values) cdefs
;;

let eval_test need_division_by_absorbent i exp =
  let test_function_ident = Longident.Lident test_function_name in
  if need_division_by_absorbent then
    Code.Pstr_eval
      (Code.apply2 test_function_ident (mk_int i)
        (Code.Try (exp, [
         Code.Clause (Code.failure_division_by_absorbent,
           Code.make_Constant "true")]))
       ) else
  Code.Pstr_eval (Code.apply2 test_function_ident (mk_int i) exp)
;;

let genr_test_body rnd num_tests val_depth ntds =
  let need_division_by_absorbent, exps =
    let bool_list, elist =
      List.split (List.map (genr_testing rnd num_tests val_depth) ntds) in
    List.mem true bool_list, List.flatten elist in
  let len = List.length exps - 1 in
  let test_numbers = Listutils.from_to !gen_num (!gen_num + len) succ in
  gen_num := !gen_num + len + 1;
  List.map2 (eval_test need_division_by_absorbent) test_numbers exps
;;

let genr_test rnd num_tests val_depth ntds =
  genr_test_body rnd num_tests val_depth ntds
;;

let genr_type_test seed num_tests val_depth ntds =
  Random.init seed;
  genr_test true num_tests val_depth ntds
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
