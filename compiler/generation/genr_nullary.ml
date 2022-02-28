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

(* $Id: genr_nullary.ml,v 1.12 2012-04-02 09:27:25 weis Exp $ *)

open Parsetree
open Genr_base
open Otype
;;

(* only rule makes sense for nullary generators *)
let genr_fun_by_ap g =
  let genr = function
    | Rewrite (pat,e) ->
      begin match pat.ppat_desc with
      | Ppat_construct (p, None, _) when p = g.pgen_desc ->
        [Code.Ocaml_expression e]
      | Ppat_construct (_, None, _)
      | Ppat_any
      | Ppat_var _
      | Ppat_alias _
      | Ppat_constant _
      | Ppat_tuple _
      | Ppat_construct _
      | Ppat_variant _
      | Ppat_record _
      | Ppat_array _
      | Ppat_or _
      | Ppat_constraint _
      | Ppat_type _
      | Ppat_lazy _
      | Ppat_parens _ ->
        failwith "Rewrite rule for a constant generator must have this \
                  generator as left hand side pattern."
      end
    | Structure_item _ | Status _ | Precedence _ -> []
    | Idempotent _ | Involutive  | Nilpotent (_, _)
    | Distributive (_, _, _, _) | Division_by_Absorbent _
    | Inverse _ | Neutral _ | Absorbent _ | Absorbing _
    | Associative _ | Commutative _ -> assert false in
  function rel -> genr rel.prel_desc
;;

let genr_function cdef =
  let g = generator_of_cdef cdef
  and relations = relations_of_cdef cdef in

  let default = Genr_base.generator0 g in

  let body =
    match relations.prels_desc with
    | Prels_none | Prels_commented _ -> default
    | Prels_begend rls ->
      let rls = order_rels Relation.precedence rls in
      match List.flatten (List.map (genr_fun_by_ap g) rls) with
      | [] -> default
      | [e] -> e
      | _ ->
        failwith "No more than one rewrite rule for a given constant \
                  constructor." in
  [Construction (g, [], body)]
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
