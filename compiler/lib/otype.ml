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

(* $Id: otype.ml,v 1.46 2012-06-04 13:01:22 weis Exp $ *)

(** {3 Functions on OCaml types.} *)

open Asttypes
open Parsetree
open Longident
;;

let opposite = function
  | Left -> Right
  | Right -> Left
  | Both -> Both
;;

(** Converting longident and generator into string. *)

let name_of_longident = function
  | Lident s
  | Ldot (_, s) -> s
  | Lapply _ -> assert false
;;

let name_of_generator g = name_of_longident g.pgen_desc
;;

let rec qualified_name_of_longident = function
  | Lident s -> s
  | Ldot (lid, s) -> Printf.sprintf "%s.%s" (qualified_name_of_longident lid) s
  | Lapply _ -> assert false
;;

let qualified_name_of_generator g = qualified_name_of_longident g.pgen_desc
;;

(** Functions building relations *)

let mk_rel desc = { prel_desc = desc; prel_loc = Location.none }
;;

(** Functions building core types. *)

let mk_type desc = { ptyp_desc = desc; ptyp_loc = Location.none; }
;;

let var_type s = mk_type (Ptyp_var s)
;;

let constr_type tname cts = mk_type (Ptyp_constr (Lident tname, cts))
;;

let poly_type tname params = constr_type tname (List.map var_type params)
;;

let constant_type tname = constr_type tname []
;;

let bool_type = constant_type "bool"
;;

let arrow_type ct1 ct2 = mk_type (Ptyp_arrow ("", ct1, ct2))
;;

let tuple_type cts = mk_type (Ptyp_tuple cts)
;;

let list_type ct = constr_type "list" [ct]
;;

let type_of_eq tname params =
  let ct = poly_type tname params in
  arrow_type ct (arrow_type ct bool_type)
;;

let projection_type tname params ctrep =
  let ct = poly_type tname params in
  arrow_type ct ctrep
;;

let injection_type tname params ctrep =
  let ct = poly_type tname params in
  arrow_type ctrep ct
;;

(** Functions testing/accessing core types. *)

let is_type_list ct =
  match ct.ptyp_desc with
   (* Should be the list constructor from the compiler:
      we should check list has not been redefined.
      But how can we do that check ? *)
  | Ptyp_constr (Lident "list", [_ct]) -> true
  | _ -> false
(** We must check if the type argument is indeed the Caml compiler
    list functional type constructor.
    Unfortunately, there is no easy way to do this... *)
;;

let argument_of_type_list = function
  | [ct] -> ct
  | _ ->
    invalid_arg
      "Otype.argument_of_type_list: not a single argument to type list"
;;

(** Syntactic equality ignoring locations *)
(* Laura's function used for testing. *)
let rec eq_type t1 t2 = eq_type_desc t1.ptyp_desc t2.ptyp_desc

and eq_type_desc td1 td2 =
  match td1, td2 with
  | Ptyp_any, Ptyp_any -> true
  | Ptyp_var v1, Ptyp_var v2 ->
     v1 = v2
  | Ptyp_arrow (l1, ct_arg1, ct_res1), Ptyp_arrow (l2, ct_arg2, ct_res2) ->
     l1 = l2 &&
     eq_type ct_arg1 ct_arg2 &&
     eq_type ct_res1 ct_res2
  | Ptyp_tuple cts1, Ptyp_tuple cts2 ->
     eq_types cts1 cts2
  | Ptyp_constr (id1, cts1), Ptyp_constr (id2, cts2) ->
     id1 = id2 &&
     eq_types cts1 cts2
  | Ptyp_object cfts1, Ptyp_object cfts2 ->
     List.length cfts1 = List.length cfts2 &&
     eq_fields cfts1 cfts2
  | Ptyp_class (id1, cts1, lbs1), Ptyp_class (id2, cts2, lbs2) ->
     id1 = id2 &&
     eq_types cts1 cts2 &&
     lbs1 = lbs2
  | Ptyp_alias (ct1, s1), Ptyp_alias (ct2, s2) ->
     ct1 = ct2 &&
     s1 = s2
  | Ptyp_variant (rfs1, b1, lbs1), Ptyp_variant (rfs2, b2, lbs2) ->
     eq_row_fields rfs1 rfs2 &&
     b1 = b2 &&
     lbs1 = lbs2
  | Ptyp_poly (ss1, ct1), Ptyp_poly (ss2, ct2) ->
     ss1 = ss2 &&
     eq_type ct1 ct2
  | Ptyp_parens t1, ct2 ->
     eq_type_desc t1.ptyp_desc ct2
  | ct1, Ptyp_parens t2 ->
     eq_type_desc ct1 t2.ptyp_desc
  | _, _ -> false

and eq_types cts1 cts2 =
  List.length cts1 = List.length cts2 &&
  List.for_all2 eq_type cts1 cts2

and eq_field cft1 cft2 = eq_field_desc cft1.pfield_desc cft2.pfield_desc

and eq_field_desc cfd1 cfd2 =
  match cfd1, cfd2 with
  | Pfield (s1, ct1), Pfield (s2, ct2) ->
      s1 = s2 &&
      eq_type ct1 ct2
  | Pfield_var, Pfield_var -> true
  | Pfield_var, Pfield _
  | Pfield _, Pfield_var -> false

and eq_fields cfs1 cfs2 =
  List.length cfs1 = List.length cfs2 &&
  List.for_all2 eq_field cfs1 cfs2

and eq_row_field rf1 rf2 =
  match rf1, rf2 with
  | Rtag (l1, b1, cts1), Rtag (l2, b2, cts2) ->
      l1 = l2 &&
      b1 = b2 &&
      eq_types cts1 cts2
  | Rinherit ct1, Rinherit ct2 ->
      eq_type ct1 ct2
  | Rtag _, Rinherit _
  | Rinherit _, Rtag _ -> false

and eq_row_fields rfs1 rfs2 =
  List.length rfs1 = List.length rfs2 &&
  List.for_all2 eq_row_field rfs1 rfs2
;;

(** Comparison on core types (ignore locations). *)
(* FIXME:
- take into account type definitions/aliases
- permutation of field names
- alpha-conversion of type variables? *)

let lex cmp1 cmp2 (x1, x2) (x1', x2') =
  match cmp1 x1 x1' with
  | 0 -> cmp2 x2 x2'
  | n -> n
;;

let compare_list cmp =
  let rec list = function
    | [], [] -> 0
    | x1 :: xs1, x2 :: xs2 ->
        begin match cmp x1 x2 with
        | 0 -> list (xs1, xs2)
        | n -> n
        end
    | [], _ -> -1
    | _, [] -> 1
  in fun xs1 xs2 -> list (xs1, xs2)
;;

let rec compare_core_type ct1 ct2 =
  match ct1.ptyp_desc, ct2.ptyp_desc with
  | Ptyp_var s1, Ptyp_var s2 -> Pervasives.compare s1 s2
  | Ptyp_arrow (l, ct1, ct2), Ptyp_arrow (l', ct1', ct2') ->
      lex Pervasives.compare (lex compare_core_type compare_core_type)
        (l, (ct1, ct2)) (l', (ct1', ct2'))
  | Ptyp_tuple cts, Ptyp_tuple cts' ->
      compare_list compare_core_type cts cts'
  | Ptyp_constr (li, cts), Ptyp_constr (li', cts') ->
      lex Pervasives.compare (compare_list compare_core_type)
        (li, cts) (li', cts')
  | Ptyp_any, Ptyp_any
  | Ptyp_object _, Ptyp_object _
  | Ptyp_class _, Ptyp_class _
  | Ptyp_alias _, Ptyp_alias _
  | Ptyp_variant _, Ptyp_variant _
    -> assert false
  | Ptyp_poly (ss, ct), Ptyp_poly (ss', ct') ->
      lex Pervasives.compare compare_core_type (ss, ct) (ss', ct')
  | Ptyp_parens ct, Ptyp_parens ct' -> compare_core_type ct ct'
  | x, x' -> Pervasives.compare x x'
;;

(** Given an association mapping variable names to core types,
applies it to the given core type list as a substitution *)

(* completion/useful should be in lib/:

let subst smap =
  let rec aux ct =
    let update desc = { ct with ptyp_desc = desc } in
    match ct.ptyp_desc with
    | Ptyp_var a -> (try Useful.StrMap.find a smap with Not_found -> ct)
    | Ptyp_arrow (l, t1, t2) ->        update (Ptyp_arrow (l, aux t1, aux t2))
    | Ptyp_tuple ts -> update (Ptyp_tuple (List.map aux ts))
    | Ptyp_constr (name, ts) ->        update (Ptyp_constr (name, List.map aux ts))
    | Ptyp_parens t -> update (Ptyp_parens (aux t))
    | Ptyp_any
    | Ptyp_object _
    | Ptyp_class _
    | Ptyp_alias _
    | Ptyp_variant _
    | Ptyp_poly _ -> ct
  in aux
;;
*)

let subst param_binding types =
  let rec app_subst ct =
    let update desc = { ct with ptyp_desc = desc } in
    match ct.ptyp_desc with
      Ptyp_any -> ct
    | Ptyp_var a ->
        (try List.assoc a param_binding with Not_found -> ct)
    | Ptyp_arrow (l, t1, t2) ->
        update (Ptyp_arrow (l, app_subst t1, app_subst t2))
    | Ptyp_tuple ts -> update (Ptyp_tuple (List.map app_subst ts))
    | Ptyp_constr (name, ts) ->
        update (Ptyp_constr (name, List.map app_subst ts))
    | Ptyp_object _ -> ct
    | Ptyp_class _ -> ct
    | Ptyp_alias (t, s) -> update (Ptyp_alias (app_subst t, s))
    | Ptyp_variant _ -> ct
    | Ptyp_poly _ -> ct
    | Ptyp_package _ -> ct
    | Ptyp_parens t -> update (Ptyp_parens (app_subst t)) in
  List.map app_subst types
;;

(** Comment relations. *)

let comment_relations_in_type_kind = function
  | Ptype_variant cdefs ->
      let comment_rels (gid, tycl, rels, loc) =
        let new_rels =
          { rels with prels_desc = Prels_commented rels } in
        (gid, tycl, new_rels, loc) in
      Ptype_variant (List.map comment_rels cdefs)
  | Ptype_record fdefs ->
      let comment_rels (fid, mflag, fty, rels, loc) =
        let new_rels =
          { rels with prels_desc = Prels_commented rels } in
        (fid, mflag, fty, new_rels, loc) in
      Ptype_record (List.map comment_rels fdefs)
  | Ptype_abstract rels ->
      let new_rels =
        { rels with prels_desc = Prels_commented rels } in
      Ptype_abstract new_rels
;;

let comment_relations_in_type_declaration td =
  { td with ptype_kind = comment_relations_in_type_kind td.ptype_kind; }
;;

(** Remove user rules. *)

let remove_rules_desc = function
  | Prels_none -> Prels_none
  | Prels_commented rs -> Prels_commented rs
  | Prels_begend rs -> Prels_begend (List.filter Relation.is_not_rewrite rs)
;;

let remove_rules rels =
  { rels with prels_desc = remove_rules_desc rels.prels_desc }
;;

let remove_rules_in_type_kind = function
  | Ptype_variant cdefs ->
      let aux (gid, tycl, rels, loc) =
        (gid, tycl, remove_rules rels, loc) in
      Ptype_variant (List.map aux cdefs)
  | Ptype_record fdefs ->
      let aux (fid, mflag, fty, rels, loc) =
        (fid, mflag, fty, remove_rules rels, loc) in
      Ptype_record (List.map aux fdefs)
  | Ptype_abstract rels ->
      Ptype_abstract (remove_rules rels)
;;

let remove_rules_in_type_declaration td =
  { td with ptype_kind = remove_rules_in_type_kind td.ptype_kind; }
;;

(** Add rules. *)

let add_rules_desc rs = function
  | Prels_none -> Prels_begend rs
  | Prels_commented _ -> Prels_begend rs
  | Prels_begend rs2 -> Prels_begend (rs2 @ rs)
;;

let add_rules rs rels =
  { rels with prels_desc = add_rules_desc rs rels.prels_desc }
;;

(** Functions on type declarations. *)

let make_public_type_declaration td =
  { td with ptype_private = Public }
;;

let get_structure_items_of_type_declaration td =

  let rec get_structure_items_of_rls accu = function
    | [] -> accu
    | { prel_desc = Structure_item struct_item; prel_loc = _ } :: rls ->
      get_structure_items_of_rls (struct_item :: accu) rls
    | { prel_desc = _; prel_loc = _ } :: rls ->
      get_structure_items_of_rls accu rls in

  let rec get_structure_items_of_relations accu rels =
    match rels.prels_desc with
    | Prels_none -> accu
    | Prels_commented rels -> get_structure_items_of_relations accu rels
    | Prels_begend rls -> get_structure_items_of_rls accu rls in

  let get_structure_items_of_generators accu cstrs =
    List.fold_left
      (fun str_items (_gid, _ctys, rels, _loc) ->
         get_structure_items_of_relations [] rels @ str_items)
      accu cstrs in

  let get_structure_items_of_fields accu flds =
    List.fold_left
      (fun str_items (_fid, _mflag, _fty, rels, _loc) ->
         get_structure_items_of_relations [] rels @ str_items)
      accu flds in

  let get_structure_items_of_type_kind = function
    | Ptype_abstract relations ->
      get_structure_items_of_relations [] relations
    | Ptype_variant cdefs ->
      get_structure_items_of_generators [] cdefs
    | Ptype_record fdefs ->
      get_structure_items_of_fields [] fdefs in

  List.rev (get_structure_items_of_type_kind td.ptype_kind)
;;

let is_private_type_declaration tdecl =
  tdecl.ptype_private = Asttypes.Private
;;

let is_variant_type_declaration tdecl =
  match tdecl.ptype_kind with
  | Ptype_variant _ -> true
  | Ptype_abstract _ | Ptype_record _ -> false
;;

let is_private_variant_type_declaration tdecl =
  is_variant_type_declaration tdecl &&
  is_private_type_declaration tdecl
;;

(** Functions on constructor definitions. *)

type constructor_definition = string * core_type list * relations * Location.t
;;

let generator_of_cdef (s, _cts, _rels, loc) =
  { pgen_desc = Longident.Lident s; pgen_loc = loc }
;;

let relations_of_cdef (_s, _cts, rels, _loc) = rels
;;

let arity_of_cdef (_s, cts, _rels, _loc) = List.length cts
;;

let has_relations rels =
  match rels.prels_desc with
  | Prels_none
  | Prels_commented _
  | Prels_begend [] -> false
  | Prels_begend _ -> true
;;

let has_relations_cdef (_s, _cts, rels, _loc) = has_relations rels
;;

let has_relations_cdefs = List.exists has_relations_cdef
;;

(* simplify and make parsable a list of relations *)

open Relation
;;

let add_left_right_rel rset r acc opp_rel both_rel =
  if RelSet.mem both_rel acc then acc else
  if RelSet.mem opp_rel rset then RelSet.add both_rel acc else
  RelSet.add r acc
;;

let left_right rset r acc =
  match r.prel_desc with
  | Neutral (Left | Right as s, g) ->
    let opp_rel = mk_rel (Neutral (opposite s, g)) in
    let both_rel = mk_rel (Neutral (Both, g)) in
    add_left_right_rel rset r acc opp_rel both_rel

  | Nilpotent (Left | Right as s, g) ->
    let opp_rel = mk_rel (Nilpotent (opposite s, g)) in
    let both_rel = mk_rel (Nilpotent (Both, g)) in
    add_left_right_rel rset r acc opp_rel both_rel

  | Absorbing (Left | Right as s, g) ->
    let opp_rel = mk_rel (Absorbing (opposite s, g)) in
    let both_rel = mk_rel (Absorbing (Both, g)) in
    add_left_right_rel rset r acc opp_rel both_rel

  | Absorbent (Left | Right as s, g) ->
    let opp_rel = mk_rel (Absorbent (opposite s, g)) in
    let both_rel = mk_rel (Absorbent (Both, g)) in
    add_left_right_rel rset r acc opp_rel both_rel

  | Distributive (Left | Right as s, g, gopt, dir) ->
    let opp_rel = mk_rel (Distributive (opposite s, g, gopt, dir)) in
    let both_rel = mk_rel (Distributive (Both, g, gopt, dir)) in
    add_left_right_rel rset r acc opp_rel both_rel

  | Idempotent (Left | Right as s) ->
    let opp_rel = mk_rel (Idempotent (opposite s)) in
    let both_rel = mk_rel (Idempotent (Both)) in
    add_left_right_rel rset r acc opp_rel both_rel

  | Inverse (Left | Right as s, g, gopt) ->
    let opp_rel = mk_rel (Inverse (opposite s, g, gopt)) in
    let both_rel = mk_rel (Inverse (Both, g, gopt)) in
    add_left_right_rel rset r acc opp_rel both_rel

  | Structure_item _
  | Status _
  | Precedence _
  | Rewrite _
  | Inverse _
  | Distributive _
  | Absorbent _
  | Absorbing _
  | Idempotent _
  | Neutral _
  | Division_by_Absorbent _
  | Nilpotent _
  | Involutive
  | Associative _
  | Commutative _ -> RelSet.add r acc
;;

let commut r =
  let new_rd =
    match r.prel_desc with
    | Neutral (_, g) -> Neutral (Both, g)
    | Nilpotent (_, g) -> Nilpotent (Both, g)
    | Absorbing (_, g) -> Absorbing (Both, g)
    | Absorbent (_, g) -> Absorbent (Both, g)
    | Distributive (_, g, gopt, dir) -> Distributive (Both, g, gopt, dir)
    | Idempotent _ -> Idempotent Both
    | Inverse (_, g, gopt) -> Inverse (Both, g, gopt)
    | Structure_item _
    | Status _
    | Precedence _
    | Rewrite _
    | Division_by_Absorbent _
    | Involutive
    | Associative _
    | Commutative _ -> r.prel_desc in
  { r with prel_desc = new_rd; }
;;

let norm rs =
  let rs =
    if List.exists is_commutative rs
    then RelSet.of_list_map commut rs else
    let rset = RelSet.of_list rs in
    RelSet.fold (left_right rset) rset RelSet.empty in
  RelSet.elements rs
;;

let norm rels =
  match rels.prels_desc with
  | Prels_begend rs ->
    { rels with
      prels_desc = Prels_begend (norm rs); }
  | Prels_none
  | Prels_commented _ -> rels
;;

(** Functions building patterns. *)

let mk_pat desc = { ppat_desc = desc; ppat_loc = Location.none; }
;;

(** Functions building expressions. *)

let mk_exp desc = { pexp_desc = desc; pexp_loc = Location.none; }
;;

let mk_when e1 e2 = mk_exp (Pexp_when (e1, e2))
and mk_apply e es = mk_exp (Pexp_apply (e, List.map (fun e -> "", e) es))
and mk_ident s = mk_exp (Pexp_ident (Lident s))
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
