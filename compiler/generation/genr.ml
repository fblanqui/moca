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

(* $Id: genr.ml,v 1.79 2012-04-02 09:27:25 weis Exp $ *)

(** {3 Generation of construction functions for a type definition.} *)

open Parsetree
open Code
open Genr_base
open Otype
open Check
;;

(** The global generation procedure:
    it generates the whole set of construction
    function definitions from a given type definition.
 *)

let _get_listary_preamble, set_listary_preamble =
  let listary_preamble = ref false in
  (fun () -> !listary_preamble),
  (fun () -> listary_preamble := true)
;;

let genr_function tname ctype cdef =
  let generator = Otype.generator_of_cdef cdef in
  let generator_info = Check.find_generator_info generator in
  let generator_arity = generator_info.gi_arity in
  let generated_functions =
    match generator_arity with
    | Ga_zeroary -> Genr_nullary.genr_function cdef
    | Ga_unary -> Genr_unary.genr_function cdef
    | Ga_binary -> Genr_binary.genr_function cdef
    | Ga_listary ->
      set_listary_preamble ();
      Genr_listary.genr_function cdef
    | Ga_mary _i -> Genr_nary.genr_function cdef in

  (* Memoize the functions if needed.
     Of course, this does not make sense in the zerorary/nullary case *)
  if Genr_memo.get_memoize_target () && (generator_arity <> Ga_zeroary)
  then Genr_memo.genr_memo_functions tname ctype cdef generated_functions
  else generated_functions
;;

let genr_record tname fields =
  let genr_field lab_name =
    (Longident.Lident lab_name, make_Constant lab_name) in
  [Construction_record (
    Longident.Lident tname,
    List.map make_Constant fields,
    Record (List.map genr_field fields))]
;;

let genr_abbrev tname _trep relations =
  (* We must generate a Module.make_tname function and a Module.from_tname
     function.

     The Module.make_tname function injects value of type [trep] into type
     [tname]; therefore, it must use the relations defined with type [tname].
     Module.make_tname is generated here.

     The Module.from_tname function just projects values from type [tname] to
     type [trep]; therefore, it is trivial and just a casting identity
     primitive.

     Module.from_tname must appear both in the interface and in the
     implementation of the module. *)
  match Relation.rewrite relations with
  | [] -> [ Construction_helper(Longident.Lident ("make_" ^ tname), [x], x)]
  | rls ->
    let clauses =
      List.map
        (fun (pat, e) ->
         match e.pexp_desc with
         | Pexp_when (whe, e) ->
           Clause_when
             (Ocaml_pattern pat, Ocaml_expression whe,
              Ocaml_expression e)
         | _ ->
           Clause (Ocaml_pattern pat, Ocaml_expression e)) rls in
    [Construction_helper (
      Longident.Lident ("make_" ^ tname), [], Function clauses)]
;;

let genr_functions_of_type_declaration (tname, td) =

  match td.ptype_kind, td.ptype_private with

  (* Public types: we do not generate construction functions. *)

  (* Regular sum type definition. *)
  | Ptype_variant cdefs, Asttypes.Public ->
    if has_relations_cdefs cdefs then
      Check.raise_error td.ptype_loc (Check.Public_type_with_relations tname)
    else []

  (* Regular record type definition. *)
  | Ptype_record _fdefs, Asttypes.Public -> []

  (* Regular abbreviation type definition. *)
  | Ptype_abstract relations, Asttypes.Public ->
    if has_relations relations then
      Check.raise_error td.ptype_loc (Check.Public_type_with_relations tname)
    else []

  (* Private types: we must generate construction functions. *)

  (* Private sum type definition: the true thing ... *)
  | Ptype_variant cdefs, Asttypes.Private ->
    let gen_fun =
      List.flatten
        (List.map (genr_function tname td.ptype_manifest) cdefs) in
    gen_fun


  (* Private record type definition. *)
  (* We generate a trivial construction function. *)
  | Ptype_record fdefs, Asttypes.Private ->
    let fields =
      List.map (fun (lab_name, _mut_flag, _ct, _rels, _loc) -> lab_name)
        fdefs in
    genr_record tname fields

  (* Private abbreviation type definition. *)
  (* When sig_item would be available in relations,
     we could (trivially) generate clever injection functions.
     For the time being, we generate a trivial [make] identity construction
     function. *)
  (* We generate a projection function. *)
  | Ptype_abstract relations, Asttypes.Private ->
    match td.ptype_manifest with
    | None -> []
    | Some trep -> genr_abbrev tname trep relations
;;

let genr_projection (tname, tdecl) =
  match tdecl.ptype_kind, tdecl.ptype_private with
  | Ptype_abstract _relations, Asttypes.Private ->
    begin match tdecl.ptype_manifest with
    | None -> []
    | Some trep -> [Genr_base.genr_projection (tname, tdecl) trep]
    end
  | Ptype_abstract _, Asttypes.Public
  | Ptype_variant _, _
  | Ptype_record _, _ -> []
;;

let add_eq ntds =
  List.map Genr_base.genr_eq
    (List.filter
      (fun (_tname, tdecl) ->
       Otype.is_private_variant_type_declaration tdecl) ntds)
;;

open Eqnrel
;;

let report_completed_equations ppf kb_result ntds =
  begin match kb_result with
  | Fail es ->
      Format.fprintf ppf
        "Knuth-Bendix completion failed.@.\
         Found the following unorientable equations:@.";
      Equation.EqnSet.fprintf ppf es;
      Format.fprintf ppf "@."
  | Incomplete when get_kb () ->
      Format.fprintf ppf
        "Knuth-Bendix completion stopped after %i steps.@."
        (get_kb_limit ())
  | Incomplete
  | Success _ -> ()
  end;
  begin match kb_result with
  | Success _ ->
      Format.fprintf ppf
        "System obtained by Knuth-Bendix completion and used by Moca:@.@."
  | Fail _
  | Incomplete ->
      Format.fprintf ppf
        "System obtained by adhoc completion and used by Moca:@.@."
  end;

  Pr_ocaml.pr_type_definitions ppf ntds;

;;

(* completion is called here *)
let complete_equations ppf ntds =
  let kb_result, not_algebraic =
    if get_kb ()
    then completion (get_kb_limit ()) ntds
    else Incomplete, false in
  if not_algebraic then
    Format.fprintf ppf
      "Warning: non algebraic rules are not taken into account \
       by the Knuth-Bendix completion.@.";
  let ntds =
    match kb_result with
    | Success ntds -> ntds
    | Fail _
    | Incomplete -> Complete.adhoc_completion ntds in

  report_completed_equations ppf kb_result ntds;
  ntds
;;

let genr_topletrec defs =
  Code.topletrec (
    List.map
      (fun d ->
       match d with
       | Construction (g, args, body) ->
         (construction_function g, args, body)
       | Construction_helper (name, args, body)
       | Construction_record (name, args, body)
       | Comparison (name, args, body) -> (name, args, body))
      defs)
;;

let genr_functions =
  let b = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer b in

  fun ntds ->

  Buffer.clear b;

  let ntds = complete_equations ppf ntds in

  let gen_functions_list =
    List.flatten (List.map genr_functions_of_type_declaration ntds) in
  let projs =
    List.flatten (List.map genr_projection ntds) in
  let eq_funs = add_eq ntds in

  let str_items =
    match gen_functions_list with
    | [] -> projs @ eq_funs
    | gen_functions_list ->
      projs @ genr_topletrec gen_functions_list :: eq_funs in

  if not (Configuration.get_verbose ()) then str_items else
  match str_items with
  | item :: items ->
    Pstr_comment ((Buffer.contents b), item) :: items
  | [] -> str_items
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
