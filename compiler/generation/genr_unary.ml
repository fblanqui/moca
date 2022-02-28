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

(* $Id: genr_unary.ml,v 1.68 2011-05-16 15:36:59 weis Exp $ *)

(** Generate code for the construction function of a ``true'' unary
    generator.
    A true unary generator has one argument and this argument is
    not of type list and should not be treated as the variable length
    list of arguments of an associative binary operation.

    The case of an associative binary operator is handled in the module
    [Genr_listary]; we branch to this case when encountering an associative
    operator, since a variable length list of argument cannot exist for a
    true unary operator. *)

open Parsetree
open Code
open Relation
open Genr_base
open Longident
open Otype
;;

let genr_function_non_listary_by_ap c =

  let rec genr = function
    | Structure_item _
    | Status _
    | Precedence _ -> []
    | Idempotent _sd ->
      [Clause (infix_name (pattern1 c underscore) "as" x,
               construction1 c x)]
    | Involutive -> [Clause (pattern1 c x, x)]
    | Nilpotent (_s, a) -> [Clause (pattern1 c underscore, construction0 a)]
    | Rewrite (p, e) -> [Genr_expression.genr_rule p e]

    | Distributive (_s, d, gopt, dflag) ->
      let e =
        match gopt with
        | Some g -> g
        | None -> d in

      (* c (d[x1, .., xn]) ->
         e [ c (x1), ..., c (xn) ] if dflag = Dist_Direct
         e [ c (xn), ..., c (x1) ] if dflag = Dist_Inverse *)
      if Check.is_listary_generator d then (* e must be listary too *)
        let idist = Lident "dist" in
        [Clause
           (pattern1 d x,
            let1 idist y (construction1 c y)
              (construction1 e
                 (apply2 (imap_of_direction dflag) (Constant idist) x))
        )]

      (* cv (d (x1, ..., xn)) ->
           e (c (x1), ..., c (xn)) if dflag = Dist_Direct
           e (c (xn), ..., c (x1)) if dflag = Dist_Inverse *)
      else (* d and e must have the same arity *)
        begin match Check.arity_of_generator d with
        | 0 -> [Clause (pattern d [], construction0 e)]
        | n ->
          let xs = genr_args "x" n in
          let lhs = pattern d xs
          and rhs =
                construction e
                  (map_of_direction dflag (construction1 c) xs) in
          [Clause (lhs, rhs)]
        end

    | Division_by_Absorbent a ->
      [
        Clause
         (pattern0 a,
          Code.failwith_division_by_absorbent);
      ]

    | Inverse _ | Neutral _ | Absorbent _ | Absorbing _
    | Associative _ | Commutative _ -> assert false in

  function rel -> genr rel.prel_desc
;;

let genr_function_non_listary cdef arg =

  let g = generator_of_cdef cdef in
  let relations = relations_of_cdef cdef in

  let default = generator1 g arg in
  match relations.prels_desc with
  | Prels_none | Prels_commented _ -> default
  | Prels_begend rls ->
    let rls = order_rels precedence rls in
    let clauses =
      List.flatten
        (List.map (genr_function_non_listary_by_ap g) rls) @
      [ Clause (underscore, default) ] in
    Match (x, clauses)
;;

let genr_function cdef =
  let g = generator_of_cdef cdef in
  let x = Code.x in
  let body = genr_function_non_listary cdef x in
  [Construction (g, [x], body)]
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
