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

(* $Id: complete.ml,v 1.91 2012-02-20 15:04:32 weis Exp $ *)

open Relation
open Parsetree
open Genr_base
open Otype
;;

(* Creates a hash table [generator name -> function definition]
   from a list of generator definitions. *)

let create_table cdefs =
  let ht = Hashtbl.create 17 in
  let enter_def (gname, core_types, rels, loc) =
    let rels = order_relations precedence rels in
      Hashtbl.add ht gname (gname, core_types, rels, loc) in
    List.iter enter_def cdefs;
    ht
;;

let get_cdef ht g = Hashtbl.find ht (Otype.name_of_generator g)
;;

let get_relations ht g = relations_of_cdef (get_cdef ht g)
;;

let add_uniq =
  fun gname r1 ->
    let print_rule () =
      Configuration.verbose_print "Constructor %s: add relation %a.\n"
        gname Pr_ocaml.pr_relation r1 in
    let print_add l = print_rule (); r1 :: l in
    let rec sorted_add = function
      | [] -> print_add []
      | (r2 :: rs) as l ->
          begin
            match compare_relation precedence r1 r2 with
            | n when n > 0 -> r2 :: sorted_add rs
            | 0 -> l
            | _ -> print_add l
          end in
    if get_user_rel_order () then print_add else sorted_add
(* Add property [r1] in a list of properties [l], only if [r1] is
   not already member of [l].
   The resulting list is sorted wrt the predicate
   [compare_relation].
   We assume that the list argument is already sorted. *)
;;

let rec add_rels gname l1 l2 =
  match l1 with
  | [] -> l2
  | r1 :: m1 -> add_rels gname m1 (add_uniq gname r1 l2)
;;

let add_rels_to_relations gname new_rels relations =
  match relations.prels_desc with
  | Prels_none | Prels_commented _ ->
    { relations with
      prels_desc = Prels_begend (add_rels gname new_rels []) }
  | Prels_begend rels ->
    { relations with
      prels_desc = Prels_begend (add_rels gname new_rels rels) }
;;

(* Function for adding properties to generator [g] in a hash table
   [generator name -> function definition]. *)

let update_props ht g new_props =
  let add_dummy_loc prop =
    { prel_desc = prop; prel_loc = Location.none; } in
  let new_rels = List.map add_dummy_loc new_props in
  let gname = Otype.name_of_generator g in
  let s, cts, rels, loc = Hashtbl.find ht gname in
  let rels = add_rels_to_relations gname new_rels rels in
  Hashtbl.replace ht gname (s, cts, rels, loc)
;;

(* Adds in [ht] the properties required by the generator [g] and its
   associated properties, according to the relation argument. *)

let update_by_ap ht cdef =

  let g = generator_of_cdef cdef in
  let relations = relations_of_cdef cdef in
  let g_is_commutative = has_commutative relations in
  let g_is_associative = has_associative relations in

  let e_opt, side_e, equal_e_opt =
    match neutral relations with
    | Some (side, e) -> Some e, side,
        (function None -> true | Some a -> compare_generator e a = 0)
    | None -> None, Both,
        (fun _ -> false) in

  let a_opt, side_a =
    match absorbent relations with
    | Some (side, a) -> Some a, side
    | None -> None, Both in

  let update = update_props ht in
  let update_if cond g rels = if cond then update_props ht g rels in
  let update_g_if cond = update_if cond g in

  fun rel -> match rel.prel_desc with

    | Neutral (side, e) when g_is_commutative && side <> Both ->
       update g [Neutral (opposite side, e)]

    | Distributive (side_c, c, _, _) ->
        if a_opt <> None then
          begin
            match inverse (get_relations ht c) with
(* this is not true if this is a fake inverse *)
            | Some (side_inv', inv', None) ->
                if side_c = side_a && opposite side_c = side_inv' then
                  update g [Distributive (side_c, inv', None, Dist_Direct)];
                if side_c = Both then
                  begin
                    match inverse relations with
                      | Some (_, inv, _) ->
                          update inv
                            [Distributive (Both, inv', None, Dist_Direct)]
                      | _ -> ()
                  end
            | _ -> ()
          end

    | Inverse (_side_inv, inv, None)
        when Check.is_local_generator inv && e_opt = None ->
        Check.raise_error g.pgen_loc (Check.No_neutral_element g.pgen_desc)

    | Inverse (side_inv, inv, e_opt')
        when Check.is_local_generator inv && equal_e_opt e_opt' ->
        (* A true inverse: we need to have a neutral element for [g]. *)
        let e = match e_opt with
          | Some e -> e
          | None -> assert false in

        (* We cannot add those derived rules, if g is not associative. *)
        update_if g_is_associative inv [
          Distributive (Both, e, None, Dist_Direct);
          Distributive (Both, g, None, Dist_Inverse);
          Involutive;
        ];

        update_g_if (eq_side side_inv side_e && side_e <> Both) [
          Neutral (opposite side_e, e);
        ];

        update_g_if (eq_side side_inv side_e && side_inv <> Both) [
          Inverse (opposite side_inv, inv, None);
        ];

        (* If [g] has an absorbent element [abs],
           this element CANNOT have an inverse in general. So:
           - we MUST generate the extra rule Division_by_Absorbent abs
           - UNLESS absorbent element [abs] is the same as the neutral
           element for [g], since in this case [abs] is its own inverse. *)
        (match a_opt with
         | Some a when compare_generator a e <> 0 || side_a <> side_e ->
             update inv [Division_by_Absorbent a]
         | _ -> ())

    | Inverse (side_inv, inv, Some e') when Check.is_local_generator inv ->
      (* Inv is a fake inverse: we treat it especially. *)

      (* We cannot add this derived rule, if g is not associative. *)
      update_if g_is_associative inv [Involutive];

      (match e_opt with
       | Some e ->
         update_if g_is_associative inv [
           Distributive (Both, e, Some e', Dist_Direct);
         ]
       | _ -> ());

      update_g_if (g_is_commutative && side_inv <> Both) [
        Inverse (opposite side_inv, inv, Some e');
      ]

    | Nilpotent _ | Inverse _
    | Division_by_Absorbent _ | Idempotent _
    | Neutral _ | Absorbent _ | Absorbing _ | Associative _ | Commutative _
    | Rewrite _ | Involutive | Precedence _ | Structure_item _
    | Status _ -> ()
;;

(* Adds in [ht] the properties required by the properties of [cdefs]. *)
(* Adhoc completion *)
let update_table ht =
  let update _g cdef =
    let relations = relations_of_cdef cdef in
    let rels =
      match relations.prels_desc with
      | Prels_none | Prels_commented _ -> []
      | Prels_begend rels -> rels in
    List.iter (update_by_ap ht cdef) rels in
  Hashtbl.iter update ht
;;

(* Adhoc completion of a list of constructor definitions. *)
let adhoc_completion_cdefs cdefs =
  let ht = create_table cdefs in
  update_table ht;
  Hashtbl.fold (fun _ cdef l -> cdef :: l) ht []
;;

let adhoc_completion_ntd (tname, td) = tname,
  match td.ptype_kind with
  | Ptype_variant cdefs ->
      { td with ptype_kind = Ptype_variant (adhoc_completion_cdefs cdefs) }
  | Ptype_record _
  | Ptype_abstract _ -> td
;;

let adhoc_completion = List.map adhoc_completion_ntd
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
