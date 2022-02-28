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

(* $Id: genr_binary.ml,v 1.140 2012-04-02 09:27:25 weis Exp $ *)

(* We treat the case of binary generators.

    Let's [g] be such a generator; we generate a binary function (here binary
    means that the function has a pair as unique argument) that normalizes
    terms that are built with [g] as their head constructor. *)

open Parsetree
open Relation
open Code
open Genr_base
open Longident
open Otype
open Check
;;

type generated_clauses = {
   clauses : clauses;
   defaults : clauses;
}
;;

(* Generate comparison functions for the given generator using its
   associated relations.

   If [g] is commutative and has an associated comparison function
   then use it to generate the three comparison functions.

   Otherwise, use use the special comparison function generated
   by the code below (See [genr_compare_function] below.)). *)
let genr_commutative_comparisons g relations =
  let comparison_option =
    match Relation.commutative relations with
    | None -> Some (Genr_base.compare_function g)
    | lopt -> lopt in
  Genr_base.genr_comparisons comparison_option
;;

(* Generation of a construction function for a binary generator,
   according to its algebraic properties. *)
let genr_clauses_by_ap g relations =

  (* The equality function for clauses about inverse/absorbing/idempotent
     reductions must be Pervasives.compare.
     We set it below as such.
  *)
  let _, equality_function, _ =
    Genr_base.pervasives_comparisons in

  (* Generating the relevant clauses of the construction function for the
     generator [g], using the relations stated by the algebraic property
     [relation]. *)

  fun ({ clauses = clauses; defaults = defaults; } as accu) relation ->

  match relation.prel_desc with

  | Absorbent (side, a) ->
    (*
      If C has Absorbent (Left, A), then
         C (A, x) -> A.

      If C has Absorbent (Right, A), then
         C (x, A) -> A.

      Absorbent (Both, A) is the conjunction of
      Absorbent (Left, A) and Absorbent (Right, A).
    *)
    (* g (a, _) -> a. *)
    let left_clause =
      Clause (pair (pattern0 a) underscore, construction0 a)
    and right_clause =
    (* g (_, a) -> a. *)
      Clause (pair underscore (pattern0 a), construction0 a) in

    { accu with
      clauses =
        clauses_of_side side left_clause right_clause clauses; }

  | Absorbing (side, d) ->
    (*
      if C has Absorbing (Left, D), then
         C (D (x, y), y) -> y,

      if C has Absorbing (Right, D), then
         C (x, D (x, y)) -> x,

      Absorbing (Both, D) is the conjunction of
      Absorbing (Left, D) and Absorbing (Right, D).
    *)
    let left_clause =
      (* g (d (x, y), y) -> y. *)
      (* g (d (x, y), z) when y = z -> y. *)
      Clause_when (
       pair (pattern2 d underscore y) z, equality_function y z,
       y)
    and right_clause =
      (* g (x, d (x, y)) -> x. *)
      (* g (x, d (z, y)) when x = z -> x. *)
      Clause_when (
       pair x (pattern2 d z underscore), equality_function x z,
       x) in

    { accu with
      clauses =
        clauses_of_side side left_clause right_clause clauses; }

  | Associative side ->
    (*
      If C has Associative (Left), then
         C (C (x, y), z) -> C (x, C (y, z)).

      If C has Associative (Right), then
         C (x, C (y, z)) -> C (C (x, y), z).

      Associative (Both) is equivalent to
      Associative (Left).
    *)
    (* g (g (x, y), z) -> g (x, g (y, z)). *)
    let left_clause =
      Clause (
       pair (pattern2 g x y) z,
       construction2 g x (construction2 g y z))
    (* g (x, g (y, z)) -> g (g (x, y), z). *)
    and right_clause =
      Clause (
       pair x (pattern2 g y z),
       construction2 g (construction2 g x y) z) in

    { accu with
      clauses =
        clauses_left_or_right side left_clause right_clause clauses; }

  | Commutative _comp ->
    (*
      If C is not associative:

       If C has Commutative (Some cmp), then the arguments of
          C are sorted in increasing order with respect to [cmp],

          C (x, y) -> C (y, x) if cmp x y > 0.

       If C is associative:

         If C has Commutative (Some cmp), then the leaves of
         C-combs are sorted in increasing order with respect to [cmp],

          C (x, C (y, z)) -> C (y, C (x, z)) if cmp x y > 0.
          C (x, y) -> C (y, x) if cmp x y > 0
                               (and y not of the form C (u, v)).
    *)
    let commutative = true
    and associative = has_associative relations in

    (* Gives the relevant comparisons for sub terms of the given generator.
       The comparisons [(lt, eq, gt)] are generated from the generator
       definition (e.g. in the commutative algebraic rule),
       or a special version for commutative using default
       Pervasives.compare aliases.
    *)
    let (_lt, _eq, gt) = genr_commutative_comparisons g relations in

    let clauses =
      if commutative && associative then
        (* g (x, g (y, z)) when x > y -> g (y, g (x, z). *)
        Clause(
          pair x (pattern2 g y z),
          If (gt x y,
              construction2 g y (construction2 g x z),
              generator2 g x (generator2 g y z)))
          :: clauses
      else clauses in

    let defaults =
      (* g (x, t) when x > y -> g (y, x). *)
      Clause_when (
        pair x y,
        gt x y,
        construction2 g y x) :: defaults in

    { clauses = clauses; defaults = defaults; }

  | Distributive (side, d, e_opt, dir) ->
    (*
       In relation Distributive (side, D, Some E, dir), the [dir] argument
       governs the order of the arguments of generator E in the right-hand
       side of the following definition rules for distributivity.

       In particular, to obtain the rules for
              Distributive (side, D, Some E, Dist_Inverse),
       take the rules for
              Distributive (side, D, Some E, Dist_Direct)
       and simply write the arguments of E in reverse order.

      If C has Distributive (Left, D, Some E, Dist_Direct), then
         if E is zeroary (then D must be zeroary),
            C (D, z) -> E,
         if E is unary (then D must be unary),
            C (D (x), z) -> E (C (x, z)),
         if E is binary (then D must be binary),
            C (D (x, y), z) -> E (C (x, z), C (y, z)),
         if E is listary:
            if D is zeroary:
               C (D, z) -> E [],
            if D is unary:
               C (D (x), z) -> E [C (x, z)],
            if D is binary:
               C (D (x, y), z) -> E [C (x, z); C (y, z)],
            if D is listary:
               C (D [x1, ..., xn], z) -> E [C (x1; z), ..., C (xn; z)],
            if D is nary:
               C (D (x1, ..., xn), z) -> E [C (x1, z), ..., C (xn, z)],
         if E is nary (then D must be nary),
            C (D (x1, ..., xn), z) -> E (C (x1, z), ..., C (xn, z)),

      If C has Distributive (Right, D, Some E, Dist_Direct), then
         C (z, D) -> E if D and E are zeroary,
         C (z, D (x1, ..., xn)) -> E (C (z, x1), ..., C (z, xn)) otherwise.

      Distributive (Both, D, Some E, dir) is the conjunction of
      Distributive (Left, D, Some E, dir) and
      Distributive (Right, D, Some E, dir).
    *)

    (*
      Distributive (side, D, None, dir) is equivalent to
      Distributive (side, D, Some D, dir).
    *)
    let e =
      match e_opt with
      | Some e -> e
      | None -> d in

    let e_arity = Check.arity_of_generator e in
    let d_arity = Check.arity_of_generator d in

    begin match e_arity with
    | 0 ->
      (*
        if E is zeroary (then D must be zeroary),
          C (D, z) -> E,
      *)
      (* g (d, _) -> e *)
      let left_clause =
        Clause (pair (pattern0 d) underscore, construction0 e)
      (* g (_, d) -> e *)
      and right_clause =
        Clause (pair underscore (pattern0 d), construction0 e) in

      { accu with
        clauses =
          clauses_of_side side left_clause right_clause clauses; }

    | 1 ->
      (*
        if E is unary (then D must be unary),
          C (D (x), z) -> E (C (x, z)),
      *)
      let left_clause =
        Clause
          (pair (pattern1 d x) z,
           construction1 e (construction2 g x z))
      and right_clause =
        Clause
          (pair z (pattern1 d x),
           construction1 e (construction2 g z x)) in

      { accu with
        clauses =
          clauses_of_side side left_clause right_clause clauses; }

    | 2 ->
      (*
        if E is binary (then D must be binary),
          C (D (x, y), z) -> E (C (x, z), C (y, z)),
      *)
      let left_clause =
        Clause
          (pair (pattern2 d x y) z,
           construction2 e
              (construction2 g x z)
              (construction2 g y z) )
      and right_clause =
        Clause
          (pair z (pattern2 d x y),
           construction2 e
              (construction2 g z x)
              (construction2 g z y) ) in

      { accu with
        clauses =
          clauses_of_side side left_clause right_clause clauses; }

    | _ when Check.is_listary_generator e ->
      (*
        if E is listary:
           if D is zeroary:
              C (D, z) -> E [],
           if D is unary:
              C (D (x), z) -> E [C (x, z)],
           if D is binary:
              C (D (x, y), z) -> E [C (x, z); C (y, z)],
           if D is listary:
              C (D [x1, ..., xn], z) -> E [C (x1; z), ..., C (xn; z)],
           if D is nary:
              C (D (x1, ..., xn), z) -> E [C (x1, z), ..., C (xn, z)],
      *)
      begin match d_arity with
      | 0 ->
        let left_clause =
          Clause (pair (pattern0 d) z, construction e [])
        and right_clause =
          Clause (pair z (pattern0 d), construction e []) in

       { accu with
         clauses =
           clauses_of_side side left_clause right_clause clauses; }

      | 1 ->
        let left_clause =
          Clause (pair (pattern1 d x) z,
                  construction e [ construction2 g x z; ])

        and right_clause =
          Clause (pair z (pattern1 d x),
                  construction e [ construction2 g z x; ]) in

        { accu with
          clauses =
            clauses_of_side side left_clause right_clause clauses; }

      | 2 ->
        let left_clause =
          Clause (pair (pattern2 d x y) z,
                  construction e
                    [ construction2 g x z;
                      construction2 g y z; ])
         and right_clause =
          Clause (pair z (pattern2 d x y),
                  construction e
                    [ construction2 g z x;
                      construction2 g z y; ]) in

        { accu with
          clauses =
            clauses_of_side side left_clause right_clause clauses; }
      | _ when Check.is_listary_generator d ->
        let idistribute = Lident "distribute"
        and xs = Code.xs in

        let left_clause =
          Clause (
            pair (pattern1 d xs) z,
            let1 idistribute y (construction2 g y z) (
              construction1 e (
                apply2 (imap_of_direction dir) (Constant idistribute) xs)
            )
          )
        and right_clause =
          Clause (
            pair z (pattern1 d xs),
            let1 idistribute y (construction2 g z y) (
              construction1 e (
                apply2 (imap_of_direction dir) (Constant idistribute) xs)
            )
          ) in

        { accu with
          clauses =
            clauses_of_side side left_clause right_clause clauses; }
      | d_arity ->
        let xis = genr_args "x" d_arity in
        let rhs distribute = construction e (map_of_direction dir distribute xis) in
        let left_clause =
          let distribute y = construction2 g y z in
          Clause (pair (pattern d xis) z, rhs distribute)
        and right_clause =
          let distribute y = construction2 g z y in
          Clause (pair z (pattern d xis), rhs distribute) in

        { accu with
          clauses =
          clauses_of_side side left_clause right_clause clauses; }
      end

    | _e_arity ->
      (*
        if E is nary (then D must be nary),
          C (D (x1, ..., xn), z) -> E (C (x1, z), ..., C (xn, z)),
      *)

      (* g (d [x1, .., xn], z) ->
         e [c (x1, z), ..., c (xn, z)] if b = Dist_Direct
         e [c (xn, z), ..., c (x1, z)] if b = Dist_Inverse *)
      if Check.is_listary_generator d then (* e must be listary too *)
        let idistribute = Lident "distribute" in
        let left_clause =
          Clause (
            pair (pattern1 d x) z,
            let1 idistribute y (construction2 g y z) (
            construction1 e (
              apply2 (imap_of_direction dir) (Constant idistribute) x)
            )
          )
        and right_clause =
          Clause (
           pair z (pattern1 d x),
           let1 idistribute y (construction2 g z y) (
             construction1 e (
               apply2 (imap_of_direction dir) (Constant idistribute) x)
           )
          ) in

        { accu with
          clauses =
            clauses_of_side side left_clause right_clause clauses; }

        (* if b = Dist_Direct then
             g (d (x1, .., xn), z) -> e (c (x1, z), .., c (xn, z))
           if b = Dist_Inverse
             e (c (xn, z), .., c (x1, z)) *)
      else (* d and e must have the same arity *)
        begin match d_arity with
        | 0 ->
          (* g (d, _) -> e *)
          let left_clause =
            Clause (pair (pattern0 d) underscore, construction0 e)
          (* g (_, d) -> e *)
          and right_clause =
            Clause (pair underscore (pattern0 d), construction0 e) in

          { accu with
            clauses =
              clauses_of_side side left_clause right_clause clauses; }
        | d_arity ->
          let xis = genr_args "x" d_arity in
          let rhs distribute = construction e (map_of_direction dir distribute xis) in
          let left_clause =
            let distribute y = construction2 g y z in
            Clause (pair (pattern d xis) z, rhs distribute)
          and right_clause =
            let distribute y = construction2 g z y in
            Clause (pair z (pattern d xis), rhs distribute) in

         { accu with
           clauses =
             clauses_of_side side left_clause right_clause clauses; }
        end
    end

  | Idempotent s ->
    let left_clause =
    (* C (C (x, y), y) -> C (x, y),
       if x and C (x, x) have the same type, then
         C (x, x) -> x, *)
      (* g ((g (_, x) as z), y) when x = y -> z *)
      Clause_when (
        pair (infix_name (pattern2 g underscore x) "as" z) y,
        equality_function x y,
        z)
    and right_clause =
    (* C (x, C (x, y)) -> C (x, y),
       if x and C (x, x) have the same type, then
       C (x, x) -> x, *)
      (* g (x, (g (y, _) as z)) when x = y -> z *)
      Clause_when (
        pair x (infix_name (pattern2 g y underscore) "as" z),
        equality_function x y,
        z) in

    (* check the condition to generate the equation c (x, x) = x *)
    (* x and C (x, x) have the same type:
       target type of C must be the type of its argument. *)
    let idempotent_condition =
      let gi = find_generator_info g in
      match gi.gi_arg_types with
      | t :: _ -> eq_type gi.gi_target_type t
      | _ -> false in

    let clause_eq =
      (* g (x, y) when x = y -> x *)
      Clause_when (
        pair x y,
        equality_function x y,
        x) in

    { accu with
      clauses =
        opt idempotent_condition
          clause_eq
          (clauses_of_side s left_clause right_clause clauses); }

  | Nilpotent (s, a) ->
    (*
      If C has Nilpotent (Left, A), then
         C (C (x, y), y) -> C (x, A),
         C (x, x) -> A,

      If C has Nilpotent (Right, A), then
         C (x, C (x, y)) -> C (A, y),
         C (x, x) -> A,

      Nilpotent (Both, A) is the conjunction of
      Nilpotent (Left, A) and Nilpotent (Right, A).
    *)
    let left_clause =
      (* C (C (x, y), y) -> C (x, A) *)
      (* g (g (x, y), z) when y = z -> g (x, a) *)
      Clause_when (
        pair (pattern2 g x y) z, equality_function y z,
        construction2 g x (construction0 a))

    and right_clause =
      (* C (x, C (x, y)) -> C (A, y) *)
      (* g (x, g (z, y)) when x = z -> g (a, y) *)
      Clause_when (
        pair x (pattern2 g z y), equality_function x z,
        construction2 g (construction0 a) y)

    and eq_clause =
      (* C (x, x) -> A *)
      (* g (x, y) when x = y -> a *)
      Clause_when (
        pair x y, equality_function x y,
        construction0 a) in

    { accu with
      clauses =
        clauses_of_side s left_clause right_clause
          (eq_clause :: clauses); }

  | Neutral (side, e) ->
    (*
      If C has Neutral (Left, E), then
         C (E, x) -> x,

      If C has Neutral (Right, E), then
         C (x, E) -> x,

      Neutral (Both, E) is the conjunction of
      Neutral (Left, E) and Neutral (Right, E).
    *)
    (* g (e, x) -> x. *)
    let left_clause = Clause (pair (pattern0 e) x, x)
    (* g (x, e) -> x. *)
    and right_clause = Clause (pair x (pattern0 e), x) in

    { accu with
      clauses =
        clauses_of_side side left_clause right_clause clauses; }

  | Inverse (side, inv, a_opt) ->
    (*
      If C has Inverse (side, I, None), then
         C must have Neutral (side, E) and
         Inverse (side, I, None) is equivalent to
         Inverse (side, I, Some E).

      If C has Inverse (Left, I, Some A), then
         I implicitely has Involutive and
         C implicitely has
           Distributive (Left, E, Some A, Dist_Direct) and

         C (I (x), x) -> A.

      If C has Inverse (Right, I, Some A), then
         I implicitely has Involutive and
         C implicitely has
           Distributive (Right, E, Some A, Dist_Direct) and

         C (x, I (x)) -> A
      Inverse (Both, I, Some A) is equivalent to
      Inverse (Left, I, Some A) and Inverse (Right, I, Some A).
    *)

    let _commutative = has_commutative relations
    and associative = has_associative relations in

    let a =
      match a_opt with
      | Some a -> a
      | None ->
        begin
          match Relation.neutral relations with
          | Some (_, e) -> e
          | None ->
            (* Should have been checked in Checking/check.ml *)
            assert false
        end in
    (* Regular rules for inverse
       L: g (inv (x), x) -> a *)
    let inv_left_clause =
      Clause_when
        (pair (pattern1 inv x) y, equality_function x y,
         construction0 a)
    (* R: g (x, inv (x)) -> a *)
    and inv_right_clause =
      Clause_when
        (pair x (pattern1 inv y), equality_function x y,
         construction0 a) in

    (* If [g] is associative then add clauses
       L: g (inv (x), g (y, z)) when x = y -> g (a, z)
       R: g (x, g (inv (y), z)) when x = y -> g (a, z).

       We also perform the additional following simplification:
       if a is in fact e, the neutral element for g, which is the common case
       for usual mathematical structures (e.g. groups, rings, and fields),
       then g (a, z) is simply z.
       This is slighly more efficient, but also simplifies the code; this is
       good for coq correction proofs! *)

    let left_clause_pattern = pair (pattern1 inv x) (pattern2 g y z)
    and right_clause_pattern = pair x (pattern2 g (pattern1 inv y) z)
    and when_cond = equality_function x y
    and clause_expr =
      match a_opt with
      | None -> z
      | Some a -> construction2 g (construction0 a) z in

    let left_cancel_clause =
      Clause_when (left_clause_pattern, when_cond, clause_expr)
    and right_cancel_clause =
      Clause_when (right_clause_pattern, when_cond, clause_expr) in

    {
      accu with
      clauses =
      let clauses =
        clauses_of_side side inv_left_clause inv_right_clause clauses in
      (if not associative then clauses else
          clauses_of_side side left_cancel_clause right_cancel_clause clauses);
    }

  | Division_by_Absorbent a ->
    (*
      If C has Division_by_Absorbent (A), then
         C (x, A) -> failwith "Division by absorbent element".
    *)
    (* g (x, a) -> failwith "Division by absorbent element" *)
    { accu with
      clauses =
        Clause (
          pair x (pattern0 a),
          Code.failwith_division_by_absorbent) :: clauses;
    }

  | Rewrite (pat, expr) ->
    (* Construction function must have the rule
       | pat -> expr
       as its first clause. *)
    { accu with
      clauses =
        Genr_expression.genr_rule pat expr :: clauses; }

  (* properties for unary generators *)
  | Involutive -> assert false
  (* other non generative properties *)
  | Structure_item _ | Precedence _ | Status _ ->
    accu
;;

(* From a construction function definition, we generate the list of clauses
   that treats the relations of the generator. *)
let clauses_of_generator g relations =

  (* We generate the regular constructor application as the
     default clause for the construction function:
     | (x, y) -> g (x, y). *)
  let default_clause =
    [ Clause (pair x y, generator2 g x y); ] in

  let default_generated_clauses =
      { clauses = []; defaults = default_clause; } in

  match relations.prels_desc with
  | Prels_none
  | Prels_commented _ ->
    default_generated_clauses
  | Prels_begend rels ->
    let rels = order_rels precedence rels in
    List.fold_left
      (genr_clauses_by_ap g relations)
      default_generated_clauses
      rels
;;

let ( @< ) = List.rev_append
;;

(* Generate a specific compare function for generator g, if needed. *)
let genr_compare_function g relations construction_function =
  (* If g is not commutative nothing to do.
     Otherwise, if the user has defined a compare function
     then use it (it should be written litterally in the generated code)
     otherwise:
       if there is no inverse, use Pervasives.compare
       otherwise, generate one in order to get an element and its inverse to
       be properly ordered and thus cancelled.
  *)
  if not (Relation.has_commutative relations)
    then [ construction_function ] else
  match Relation.commutative relations, Relation.inverse relations with
  | Some _, _ -> [ construction_function ]
  | None, None ->
    let compare_function =
      Comparison
        (compare_function g, [],
         Var (Ldot (Lident "Pervasives", "compare"))) in
    [ compare_function; construction_function; ]
  | None, Some ( _side, ginv, _greduce) ->
    let compare_function =
      let args = [x; y;] in
      let compare_lid = compare_function g in
      let compare_body =
        Match ( Tuple args,
                [ Clause (pair (pattern1 ginv x) (pattern1 ginv y),
                          apply2 compare_lid x y);
                  Clause (pair (pattern1 ginv x) y,
                          apply2 compare_lid x y);
                  Clause (pair x (pattern1 ginv y),
                          apply2 compare_lid x y);
                  Clause (pair x y,
                          apply2 (Ldot (Lident "Pervasives", "compare"))
                            x y);
                ]) in
      Comparison (compare_lid, args, compare_body) in
    [ compare_function; construction_function; ]
;;

(* Generating the construction function for a binary generator.

   The (binary) generator is given by [cdef].

   We generate the clauses according to the relations recorded with the
   generator, and add a default insert clause. *)

let genr_construction_function g relations =

  (* Arguments of [construction_function_name g] *)
  let args = [ z; ] in

  let generated_clauses =
    clauses_of_generator g relations in

  let clauses =
    generated_clauses.clauses @< generated_clauses.defaults in

  let annotated_clauses =
    if Debug.get_debug () then clauses else clauses in

  let body = Match (z, annotated_clauses) in
  Construction (g, args, body)
;;

(* Generating the recursive definition that defines the
construction function for generator [g]. *)

let genr_function cdef =

  let g = generator_of_cdef cdef
  and relations = relations_of_cdef cdef in

  let construction_function =
    genr_construction_function g relations in

  genr_compare_function g relations construction_function
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
