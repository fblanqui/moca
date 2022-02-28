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

(* $Id: check.ml,v 1.98 2012-06-04 13:01:22 weis Exp $ *)

(** {3 Builds various htables for generator and types.} *)

open Parsetree
open Relation
open Longident
open Otype
;;

(** Generator information. *)

type locality =
   | Local
   | Global of string
(** The locality flag for a generator: is it defined in the local module or
    globally ? *)
;;

type generator_arity =
   | Ga_zeroary
   | Ga_unary
   | Ga_binary
   | Ga_listary
   | Ga_mary of int
(** Constructor arity: could be
 - [Ga_zeroary] for constant or 0-ary constructors,
 - [Ga_unary] for unary functional constructors,
 - [Ga_binary] for binary functional constructors,
 - [Ga_listary] for variable arity functional constructors (those have a single
   argument which is a list of elements of the relational type at hand),
 - [Ga_mary n] for an arbitrary number of arguments [n].
   Note that:
   - [n] can be [1] when the generator cannot be considered as having a
     variable arity,
   - [n] cannot be 0 or 2,
   - [n] can be greater than 3.
*)
;;

type generator_info = {
  gi_type_name : string;
  gi_type_params : string list;
  gi_target_type : core_type;
  gi_generator_name : string;
  gi_locality : locality;
  gi_arity : generator_arity;
  gi_arg_types : core_type list;
  gi_relations : relations;
  gi_location : Location.t;
  gi_priv_flag : Asttypes.private_flag;
}
;;

(** Possible errors. *)
type msg = string;;
type error =
   | Unknown_generator of Longident.t
   | No_neutral_element of Longident.t
   | Public_type_with_relations of string
   | Cannot_find_module of Longident.t
   | Not_supported of string
   | Relation_already_declared of Location.t
   | Incompatible_types of Longident.t * Longident.t
   | Invalid_pattern
   | Pattern_not_headed_by_correct_generator of Longident.t
   | Illegal_relation of Longident.t
   | Incompatible_relations of
       Longident.t * Parsetree.relation * Parsetree.relation
   | Unsound_ordering of msg
;;

exception Error of Location.t * error
;;

let raise_error loc err = raise (Error (loc, err))
;;

let unsound_ordering msg loc =
  raise_error loc (Unsound_ordering msg)
;;

let unknown_generator g =
  raise_error g.pgen_loc (Unknown_generator g.pgen_desc)
;;

let relation_already_declared r1 r2 =
  raise_error r1.prel_loc (Relation_already_declared r2.prel_loc)
;;

let no_neutral_element_error g =
  raise_error g.pgen_loc (No_neutral_element g.pgen_desc)
;;

let incompatible_relations gname gloc r1 r2 =
  raise_error gloc (Incompatible_relations (gname, r1, r2))
;;

(** Error messages. *)

open Format
;;

let report_error ppf = function
  | Unknown_generator li ->
      fprintf ppf "Unknown generator %a."
        Pr_ocaml.pr_ident_in_prefix_position li
  | No_neutral_element li ->
      fprintf ppf "%a has an inverse but no neutral element."
        Pr_ocaml.pr_ident_in_prefix_position li
  | Public_type_with_relations s ->
      fprintf ppf "%s is a public type with relations." s
  | Cannot_find_module li ->
      fprintf ppf "Cannot find module %a in search path."
        Pr_ocaml.pr_ident_in_prefix_position li
  | Not_supported s ->
      fprintf ppf "%s not supported." s
  | Relation_already_declared loc ->
      fprintf ppf "Relation already declared at %a." Location.print loc
  | Incompatible_types (li1, li2) ->
      fprintf ppf
        "Generators %a and %a have incompatible types."
        Pr_ocaml.pr_ident_in_prefix_position li1
        Pr_ocaml.pr_ident_in_prefix_position li2
  | Invalid_pattern ->
      fprintf ppf "Invalid pattern."
  | Pattern_not_headed_by_correct_generator li ->
      fprintf ppf "Pattern not headed by %a."
        Pr_ocaml.pr_ident_in_prefix_position li
  | Illegal_relation li ->
      fprintf ppf "Relation incompatible with the type of %a."
        Pr_ocaml.pr_ident_in_prefix_position li
  | Incompatible_relations (li, rel1, rel2) ->
    fprintf ppf "Relations %a and %a are incompatible in generator %a."
      Pr_ocaml.pr_relation rel1
      Pr_ocaml.pr_relation rel2
      Pr_ocaml.pr_ident_in_prefix_position li
  | Unsound_ordering msg ->
    fprintf ppf "Unsound ordering in generator definition: %s." msg
;;

let is_local_info ginfo = ginfo.gi_locality = Local
;;

let is_private_info ginfo =
  ginfo.gi_priv_flag = Asttypes.Private
;;

let is_listary_info info = info.gi_arity = Ga_listary;;

let arity_of_info info =
  match info.gi_arity with
  | Ga_zeroary -> 0
  | Ga_unary -> 1
  | Ga_binary -> 2
  | Ga_listary -> 1
  | Ga_mary n -> n
;;


(* Lowest precedence generators, according to their arity,
   must be declared first in the corresponding .mlm file.

   As we do not know yet what is the precedence of
   listary generators,
   we choose to force them to be declared at the end.

   This is linked to the fact that we would like that Pervasives.compare
   correctly orders the term constructed by the generators, according to the
   "intuitive" semantics.
*)
let arity_precedence generator_arity =
  match generator_arity with
    | Ga_zeroary -> 0
    | Ga_unary -> 1
    | Ga_binary -> 2
    | Ga_mary n -> n
    | Ga_listary -> max_int
;;

let is_truly_unary_info info = info.gi_arity = Ga_unary;;

(** Generator table. *)

let (generator_name_tbl : (Longident.t, generator_info) Hashtbl.t) =
  Hashtbl.create 97
;;

let is_generator_ident = Hashtbl.mem generator_name_tbl;;

let find_generator_ident_info = Hashtbl.find generator_name_tbl;;

let find_generator_name_info s = find_generator_ident_info (Lident s);;

let find_generator_info g =
  try find_generator_ident_info g.pgen_desc with
  | Not_found -> unknown_generator g
;;

(** Various access/test functions. *)
let is_listary_generator g =
  is_listary_info (find_generator_info g)
;;

let is_local_generator g =
  is_local_info (find_generator_info g)
;;

let is_private_generator g =
  is_private_info (find_generator_info g)
;;

let arity_of_generator g =
  arity_of_info (find_generator_info g)
;;


let get_arity_of_generator g =
  let info = (find_generator_info g) in
  info.gi_arity
;;

let is_truly_unary_generator g =
  is_truly_unary_info (find_generator_info g)
;;

let neutral_element_of_generator g =
  let info = find_generator_info g in
  match neutral info.gi_relations with
  | Some (_sd, e) -> e
  | _ -> no_neutral_element_error g
;;

(** Check if a generator is both nilpotent and idempotent *)
let check_relations_compatibility generator_name =
  let info = find_generator_ident_info generator_name in
  let relations = info.gi_relations in
  if Relation.has_idempotent_and_nilpotent relations then
    let relation_list = Relation.get_relation_list relations in
    let r1 = Relation.find_relation is_idempotent relation_list
    and r2 = Relation.find_relation is_nilpotent relation_list in
    incompatible_relations generator_name info.gi_location r1 r2
;;

(** Type table. *)

let type_name_tbl = Hashtbl.create 19;;

let is_type_ident = Hashtbl.mem type_name_tbl;;

let find_type_ident_declaration = Hashtbl.find type_name_tbl;;

let find_type_name_declaration s = find_type_ident_declaration (Lident s);;

(** Check if a type declaration is admissible. *)

(* We should check basic things like:
   - a relation is declared only once
   - there is no two commutative's
   More complex checks should be done in the completion procedure.
*)

let always_acceptable_relation rel =
  match rel.prel_desc with
  | Division_by_Absorbent _
  | Associative _
  | Commutative _
  | Neutral _
  | Absorbent _
  | Absorbing _
  | Inverse _
  | Involutive
  | Distributive _
  | Nilpotent _
  | Idempotent _ -> false
  | Structure_item _
  | Status _
  | Precedence _
  | Rewrite _ -> true
;;

let is_possible_for_zeroary = always_acceptable_relation;;

let is_possible_for_truly_unary rel =
  match rel.prel_desc with
  | Associative _
  | Commutative _
  | Neutral _
  | Absorbing _
  | Absorbent _
  | Inverse _ -> false
  | Division_by_Absorbent _
  | Involutive
  | Distributive _
  | Nilpotent _
  | Idempotent _
  | Structure_item _
  | Status _
  | Precedence _
  | Rewrite _ -> true
;;

let is_possible_for_binary rel =
  match rel.prel_desc with
  | Associative _
  | Commutative _
  | Neutral _
  | Absorbent _
  | Absorbing _
  | Inverse _
  | Distributive _
  | Nilpotent _
  | Idempotent _
  | Structure_item _
  | Precedence _
  | Rewrite _
  | Status _ -> true
  | Involutive
  | Division_by_Absorbent _ -> false
;;

let is_possible_for_listary = is_possible_for_binary;;

(* FIXME: should be exactly the same as for binary generators!

In gen_listary.ml we find instead:

   The following things do not make sense when used in a listary way
   See moca/compiler/ocaml_src/parsing/parsetree.mli for semantic hints

  | (Division_by_Absorbent _
  | Absorbing _ (*FIXME?*)
  | Involutive) -> assert false

let is_possible_for_listary =
  function rel ->
  match rel.prel_desc with
  | Division_by_Absorbent _
  | Absorbent _
  | Absorbing _ -> false
  | Involutive
  | Associative _
  | Commutative _
  | Neutral _
  | Inverse _
  | Distributive _
  | Nilpotent _
  | Idempotent _
  | Structure_item _
  | Precedence _
  | Rewrite _
  | Status _ -> true
;;
*)

let is_possible_for_mary = always_acceptable_relation;;

let rec insert_relation r1 = function
  (* 2nd argument is assumed to be sorted wrt the compare_relation. *)
  | [] -> [r1]
  | (r2 :: rs) as l ->
    begin
      match Relation.compare_relation precedence r1 r2 with
      | 0 -> relation_already_declared r1 r2
      | n when n > 0 -> r2 :: insert_relation r1 rs
      | _ -> r1 :: l
    end
;;

let have_compatible_types d e =
  is_listary_generator e ||
  not (is_listary_generator d) &&
  arity_of_generator e = arity_of_generator d
;;

let generator_relation_are_compatible gi r =
  match gi.gi_arity with
  | Ga_zeroary -> is_possible_for_zeroary r
  | Ga_unary -> is_possible_for_truly_unary r
  | Ga_binary -> is_possible_for_binary r
  | Ga_listary -> is_possible_for_listary r
  | Ga_mary _n -> is_possible_for_mary r
;;

let check_relations generator_name =
  let gi = find_generator_name_info generator_name
  and gid = Lident generator_name in

  let rec aux (stat, prec, ass, com, builtins) = function
    | [] -> ()
    | r :: rs ->
      begin
        let error e = raise_error r.prel_loc e in
        if not (generator_relation_are_compatible gi r) then
          error (Illegal_relation gid);
        match r.prel_desc, prec, ass, com with
        | Precedence _, None, _, _ -> aux (stat, Some r, ass, com, builtins) rs
        | Precedence _, Some p, _, _ -> relation_already_declared p r
        | Status _, None, _, _ -> aux (Some r, prec, ass, com, builtins) rs
        | Status _, Some p, _, _ -> relation_already_declared p r
        | Associative _, _, None, _ ->
            aux (stat, prec, Some r, com, builtins) rs
        | Associative _, _, Some a, _ -> relation_already_declared a r
        | Commutative _, _, _, None ->
            aux (stat, prec, ass, Some r, builtins) rs
        | Commutative _, _, _, Some c -> relation_already_declared c r
        | Rewrite ({ ppat_desc = Ppat_construct (li, _, _);
                     ppat_loc = _ }, _), _, _, _
            when gid <> li ->
            error (Pattern_not_headed_by_correct_generator gid)
        | Rewrite ({ ppat_desc = Ppat_construct _; ppat_loc = _ }, _), _, _, _
        | Structure_item _, _, _, _ -> aux (stat, prec, ass, com, builtins) rs
        | Rewrite _, _, _, _ -> error Invalid_pattern
        | Distributive (_s, d, Some e, _b), _, _, _
            when not (have_compatible_types d e) ->
            error (Incompatible_types (d.pgen_desc, e.pgen_desc))
        | (Idempotent _ | Nilpotent _), _, _, _ ->
          check_relations_compatibility gid
        | _ -> aux (stat, prec, ass, com, insert_relation r builtins) rs
      end in

  fun rels ->
    match rels.prels_desc with
    | Prels_none
    | Prels_commented _ -> ()
    | Prels_begend rs -> aux (None, None, None, None, []) rs
;;

let check_generator_def (generator, _cts, rels, _loc) =
  check_relations generator rels
;;

let compute_generator_arity cts =
  match cts with
  | [] -> Ga_zeroary
  | [t] -> if Otype.is_type_list t then Ga_listary else Ga_unary
  | [_; _] -> Ga_binary
  | _ -> Ga_mary (List.length cts)
;;

(* Check that generators are declared in increasing precedence
   in a .mlm file
*)
let check_generator_order cdefs =
  let rec aux last_precedence last_cname cdefs =
      match cdefs with
    | [] -> ()
    | (cname, cts, _rels, loc) :: cdefs ->
      let precedence = arity_precedence (compute_generator_arity cts) in
      if precedence < last_precedence
      then
        let msg = sprintf
          "Constructor %s with precedence %d must be declared before \
            generator %s with precedence %d"
          cname precedence last_cname last_precedence
        in unsound_ordering msg loc
      else aux precedence cname cdefs
  in aux 0 "" cdefs
;;


let check_type_decl (_type_name, td) =
  match td.ptype_kind with
  | Ptype_variant cdefs ->
    List.iter check_generator_def cdefs;
    check_generator_order cdefs;
  | Ptype_record _
  | Ptype_abstract _ -> ()
;;

let check_type_decls = List.iter check_type_decl;;

(** Add type declarations in the tables. *)

let add tbl local_flag (generator_name, x) =
  Hashtbl.add tbl (Lident generator_name) x;
  match local_flag with
  | Local -> ()
  | Global s -> Hashtbl.add tbl (Ldot (Lident s, generator_name)) x
;;

let fill_type_name_tbl = add type_name_tbl
;;


let mk_core_type tdesc = { ptyp_desc = tdesc; ptyp_loc = Location.none }
;;

let var_type s = mk_core_type (Ptyp_var s)
;;

let target_type type_name td =
  mk_core_type (Ptyp_constr (Lident type_name, List.map var_type td.ptype_params))
;;

let fill_generator_name_tbl local_flag (type_name, td) =
  match td.ptype_kind with
  | Ptype_variant cdefs ->
    let add_cdef (generator_name, cts, rels, loc) =
      let generator_arity = compute_generator_arity cts in
      let info = {
        gi_type_name = type_name;
        gi_type_params = td.ptype_params;
        gi_target_type = target_type type_name td;
        gi_generator_name = generator_name;
        gi_arity = generator_arity;
        gi_locality = local_flag;
        gi_arg_types = cts;
        gi_relations = rels;
        gi_location = loc;
        gi_priv_flag = td.ptype_private;
      } in
      add generator_name_tbl local_flag (generator_name, info) in
    List.iter add_cdef cdefs
  | Ptype_record _
  | Ptype_abstract _ -> ()
;;

(* First add all data in the tables, then check the data
   since some checks need the table to be filled (eg. neutral element).
*)

let add_type_decls local_flag ntds =
  List.iter
    (fun ntd ->
       fill_type_name_tbl local_flag ntd;
       fill_generator_name_tbl local_flag ntd)
    ntds;
  check_type_decls ntds
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
