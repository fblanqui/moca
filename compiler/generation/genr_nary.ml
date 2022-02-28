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

(* $Id: genr_nary.ml,v 1.23 2012-04-02 09:27:25 weis Exp $ *)

(** {3 Generation of construction functions for n-ary generators.} *)

open Parsetree
open Code
open Genr_base
open Relation
open Otype
;;

let genr_clauses_by_ap _cdef =

  (* Generating the relevant clauses of the construction function for
     the generator given by [cdef], using the relations stated by the
     algebraic property [relation]. *)

  function relation ->

  match relation.prel_desc with

  | Rewrite (pat, expr) -> [Genr_expression.genr_rule pat expr]

  | Structure_item _ -> []
    (* Must not fail for structure_items as relations: this allows the
       user to define a comparison for instance. *)
  | Precedence _ -> []
  | Status _ -> []
  | Commutative _
  | Idempotent _
  | Nilpotent _
  | Associative _
  | Neutral _
  | Inverse _
  | Distributive _
  | Absorbent _
  | Absorbing _
  | Division_by_Absorbent _
  | Involutive ->
      Check.raise_error relation.prel_loc
        (Check.Not_supported "For n-ary generators, algebraic relations are")
;;

let flat_map_end f l accu =
  List.rev (
   List.rev_append accu
     (List.fold_left
       (fun ll l -> List.rev_append (f l) ll)
       []
       l)
  )
;;

let clauses_of_generator cdef default_clauses =
  let relations = relations_of_cdef cdef in
  match relations.prels_desc with
  | Prels_none | Prels_commented _ -> default_clauses
  | Prels_begend rels ->
    let rels = order_rels precedence rels in
    flat_map_end (genr_clauses_by_ap cdef) rels default_clauses
;;

let genr_construction_function cdef =
  let g = generator_of_cdef cdef in
  let args = [z] in
  let elts = Code.genr_args "x" (arity_of_cdef cdef) in
  let default_pattern = Tuple elts in

  let default_clause = Clause (default_pattern,
                               Genr_base.generator g elts) in
  let clauses = clauses_of_generator cdef [default_clause] in

  [Construction (g, args, Match (z, clauses))]
;;

let genr_function = genr_construction_function;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
