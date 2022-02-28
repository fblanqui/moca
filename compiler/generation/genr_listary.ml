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
(*  This file is distriuted under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: genr_listary.ml,v 1.48 2012-04-02 14:56:13 bonichon Exp $ *)

(* We treat the case of listary or vary-adic generators. *)

open Check
open Relation
open Genr_base
open Code
open Parsetree
open Longident
open Otype
;;

(* Generation of prefixed identifiers to define requeted functions to handle
   a listary generator. *)
let insert_in = Genr_base.prefix_construction_function "insert_"
and delete_in = prefix_construction_function "delete_"
and cons_in = prefix_construction_function "cons_"
and flat_in = prefix_construction_function "flat_"
and return_in = prefix_construction_function "return_"
and normalize_in = prefix_construction_function "normalize_"
and distrib_in = prefix_construction_function "distrib_"
and potent_in = prefix_construction_function "potent_"
and mhead_in = prefix_construction_function "matching_heads_"
;;

let apply_cons g = apply2 (cons_in g)
and apply_return g = apply1 (return_in g)
and apply_insert g = apply2 (insert_in g)
and apply_delete g = apply2 (delete_in g)
and apply_mhead g = apply2 (mhead_in g)
and apply_potent g = apply (potent_in g)
;;

(* Some useful arithmetic operations *)
let plus x y = infix_apply x (Lident "+") y
and div x y = infix_apply x (Lident "/") y
and is_gt x y = infix_apply x (Lident ">") y
and two = make_Constant "2"
and one = make_Constant "1"
;;

(* Useful Code ident expressions for this module. *)
let ident_l = Lident "l"
and ident_l1 = Lident "l1"
and ident_ys = Lident "moca_ys"
and ident_xs = Lident "moca_xs"
and ident_accu = Lident "moca_accu"
and ident_f = Lident "f"
and ident_rfm = Lident "rev_flat_map"
and ident_l1len = Lident "l1len"
and ident_maxlen = Lident "maxlen"
;;

let l = Constant ident_l
and maxlen = Constant ident_maxlen
and l1 = Constant ident_l1
and l1len = Constant ident_l1len
and ys = Constant ident_ys
and xs = Constant ident_xs
and accu = Constant ident_accu
and f = Constant ident_f
;;

let none = make_Constant "None"
and some = apply1 (Lident "Some")
;;

let assert_false = make_Constant "assert false"
;;

(* Create a Code ident for the Caml List module. *)
let list_ident s = Ldot (Lident "List", s)
;;

(* Useful function from the Caml List module. *)
let list_sort = apply2 (list_ident "sort")
and list_map = apply2 (list_ident "map")
and list_rev = apply1 (list_ident "rev")
and list_length = apply1 (list_ident "length")
;;

(* To build the code that reverses the [accu] argument. *)
let rev_accu = list_rev accu
;;

let fail s =
  failwith (Printf.sprintf "Forbidden keyword for listary generator %s" s)
;;

let nyimpl_listary s =
  Debug.not_yet_implemented (Printf.sprintf "Listary code for %s" s)
;;

let gen_hd_pattern head tail = cons head tail ;;
let list_hd_pattern g =  gen_hd_pattern g xs ;;
let list_hd_underscore g = gen_hd_pattern g underscore ;;

let list_hd_patterns l =
  let l = List.rev l in
  List.fold_left (fun x y -> cons y x) xs l
;;

let trace_clauses clauses =
  List.map
    (fun (line, file, cl) ->
     Genr_base.trace_clause line file cl) clauses
;;

(* Coercion of default clauses into the #line triple.
   As these clauses are not generated in relation with any keyword/rule,
   they do not have a provenance (originating line = 0, file = "")
*)
let added_clauses clauses =
  List.map (fun clause -> 0, "", clause) clauses
;;

(*
  Auxiliary functions to deal with non-commutative idem/nil-potence
  This function asserts that at least one of these relations has been defined.
*)
let genr_potent g relations =
  let idempotent = has_idempotent relations
  and nilpotent = has_nilpotent relations in

  let _lt, eq, _gt =
    genr_comparisons (Relation.commutative relations) in

  let matching_heads g eq =
    let mhead_ident = mhead_in g
    and args = [ l1; l; ]
    and aux_ident = Lident "aux" in

    let body =
      Letrec (aux_ident, [ys; xs],
      Match (
        Tuple [ys; xs], [
        Clause (
          Tuple [cons x xs; cons y ys],
          If (eq x y, apply aux_ident [xs; ys], none));
        Clause (
          Tuple [nil; underscore],
          if idempotent then some l else
          begin
            assert nilpotent;
            match Relation.nilpotent relations with
            | Some (_, ng) ->
               some (apply_insert g (construction0 ng) xs)
            | None -> assert false
          end);
        Clause (
          Tuple [underscore; nil],
          assert_false);
        ]),
      apply aux_ident args) in

    Construction_helper (mhead_ident, args, body) in

  let potent_ident = potent_in g
  and args = [ l1; l1len; l; maxlen; ] in

  let body =
    If (
       is_gt l1len maxlen,
       raise_error "Not_found",
       Match (l, [
         Clause (nil, raise_error "Not_found");
         Clause (
           cons y ys,
           Match (apply_mhead g l1 l, [
             Clause (some xs, xs);
             Clause (
               none,
               apply_potent g [
                 append l1 (singleton y); plus l1len one; ys; maxlen;
               ])
           ]));
       ])
    ) in


  [ Construction_helper (potent_ident, args, body); matching_heads g eq; ]
;;

(* Generate a prelude of clauses for the insert function
*)
let genr_pre_insert g relations cont =

  let genr_pre_insert_clauses_by_ap _g rel =
    let ap = rel.prel_desc in
    match ap with
    | Neutral (_sd, ng) -> [ Clause (generator0 ng, l); ]
    | Associative _ -> []
    | Inverse (_sd, _ig, _eg) -> []
    | Idempotent _sd -> []
    | Nilpotent (_sd, _ng) -> []
    | Distributive (sd, _g, _hopt, _dist) ->
      begin
        match sd with
        | Right | Left -> fail "distributive right/left"
        | Both -> []
      end

    | Absorbent ( _sd, ag) ->
        [ Clause (generator0 ag, singleton (construction0 ag)); ]
    | Commutative _ -> []
    | Rewrite _ | Structure_item _ | Precedence _ |  Status _ -> []

      (* These relations do not make sense in listary generators
         as specified in parsetree.mli. *)
    | Division_by_Absorbent _ -> fail "division by absorbent"
    | Absorbing _ -> fail "absorbing"
    | Involutive -> fail "involutive" in

  let relation_list = Relation.get_relation_list relations in

  let clauses_by_ap =
    List.flatten (List.map (genr_pre_insert_clauses_by_ap g) relation_list) in

  if clauses_by_ap = [] then cont else

  let default_clause = Clause (underscore, cont) in

  Match (x, clauses_by_ap @ [ default_clause ])
;;

let genr_insert g relations =

  let args = [ x; l; ]
  and insert_ident = insert_in g in

  let _idempotent = Relation.has_idempotent relations
  and commutative = Relation.has_commutative relations
  and _associative = Relation.has_associative relations
  and _nilpotent = Relation.has_nilpotent relations in

  let _lt, eq, gt =
    genr_comparisons (Relation.commutative relations) in

  let genr_insert_clause_by_ap g rel =
    let ap = rel.prel_desc in
    let line, filename = Genr_base.diese_line rel.prel_loc in

    let default_clause =
       Clause
         (cons underscore underscore,
          Try
            (apply_potent g [
               singleton x; one; l; div (plus (list_length l) one) two;
             ],
             [ Clause (Constant (Lident "Not_found"), cons x l); ]
            )
          ) in

    let gen_clause insert_arg1 =
      if commutative then
        Clause_when
          (cons y ys, eq x y,
           apply_insert g insert_arg1 ys)
      else default_clause in

    let cls_of_ap =
      match ap with
      | Neutral (_sd, _eg) ->  []
      | Associative _ -> []
      | Inverse (_sd, _ig, _eg) -> []
      | Idempotent _sd -> [ gen_clause x ]
      | Nilpotent (_sd, ng) -> [ gen_clause (construction0 ng) ]
      | Distributive (sd, _g, _hopt, _dist) ->
          begin
            match sd with
            | Right | Left -> fail "distributive right or left only."
            (* FIXME: is it [] or not yet implemented ??? *)
            | Both -> [] (*nyimpl_listary "distributive"*)
          end

      | Absorbent ( _sd, _ag) -> []

      | Commutative _ ->
          let clause =
            Clause_when
              (cons y ys, gt x y,
               apply_insert g y (apply_insert g x ys)) in
          [ clause ]
      | Rewrite _ | Structure_item _ | Precedence _ |  Status _ -> []
        (* These relations do not make sense in listary generators
           as specified in parsetree.mli. *)
      | Division_by_Absorbent _ -> fail "division by absorbent"
      | Absorbing _ -> fail "absorbing"
      | Involutive -> fail "involutive" in

    List.map (fun clause -> line, filename, clause) cls_of_ap in

  let relation_list = Relation.get_relation_list relations in
  let clauses_by_ap =
    List.flatten (List.map (genr_insert_clause_by_ap g) relation_list) in
  let default_clauses =
    added_clauses
      [ Clause (underscore, cons x l); ] in
  let clauses = trace_clauses (clauses_by_ap @ default_clauses) in

  let body = genr_pre_insert g relations (Match (l, clauses)) in

  Construction_helper (insert_ident, args, body)
;;

(* Clause(s) generated according to relations of the current
 generators : [] means there is nothing to do. *)
let delete_call g x z  cont  =
   Let
    (ident_ys, [],
     Try (apply2 (delete_in g) x accu,
          [Clause
             (Constant (Lident "Not_found"),
             apply_insert g z accu)]),
     cont)
;;

let genr_cons g relations =
  let cons_ident = cons_in g
  and args = [ accu; l; ]
  and relation_list = get_relation_list relations in

  let _lt, eq, _gt =
    genr_comparisons (Relation.commutative relations) in

  let commutative = has_commutative relations in

  let genr_clause_by_ap g rel =
    let ap = rel.prel_desc in
    let return = apply_return g in
    let line, filename = Genr_base.diese_line rel.prel_loc in

    let cls_of_ap =
      match ap with
      | Neutral (sd, eg) ->
        begin
          let clause_left =
            Clause (
              list_hd_patterns [ x; generator0 eg; ],
              apply_cons g accu (cons x xs))
          and clause_right =
            Clause (
              list_hd_patterns [ generator0 eg; x; ],
              apply_cons g accu (cons x xs)) in
          match sd with
            | Right -> [ clause_right ]
            | Both -> [ clause_left; clause_right ]
            | Left -> [ clause_left ]
        end

      | Associative _ ->
        (* Side does not matter in associative listary generators *)
        let clause =
          Clause (
            list_hd_pattern (pattern g [ys]),
            apply_cons g accu (append ys xs)
        ) in [ clause ]

      | Inverse (sd, ig, eg) when not commutative ->
        begin
          let reducee_g =
            match eg with
              | Some g -> g
              | None ->
                (assert (Relation.has_neutral relations);
                 match Relation.neutral relations with
                   | Some (sdn, g) ->
                     (assert (sdn = sd); g)
                   | None -> assert false
                )
          in
          let clause_right =
            Clause_when(
              list_hd_patterns [pattern ig [x]; y;], eq x y,
              apply_cons g accu (cons (generator0 reducee_g) xs))
          and clause_left =
            Clause_when(
              list_hd_patterns [x; pattern ig [y];], eq x y,
              apply_cons g accu (cons (generator0 reducee_g) xs))
          in
          match sd with
            | Right -> [ clause_right ]
            | Both -> [ clause_left; clause_right ]
            | Left -> [ clause_left ]
        end


      | Inverse ((Right | Left | Both), ig, _eg) ->
        let clause =
          Clause (
            list_hd_pattern (infix_name (pattern ig [x]) "as" z),
            delete_call g x z (apply_cons g ys xs)
        ) in
        [ clause ]

      | Idempotent _sd ->
        let commutative_clause =
          Clause_when (
            list_hd_patterns [x; y], eq x y,
            apply_cons g accu (cons x xs)
        ) in

        if commutative then [ commutative_clause ] else []

      | Nilpotent (_sd, ng) ->
        let commutative_clause =
          Clause_when (
            list_hd_patterns [x; y], eq x y,
            apply_cons g accu (cons (generator0 ng) xs)
        ) in

        if commutative then [ commutative_clause ] else []

      | Distributive (sd, dg, egopt, _dist) ->
        begin
          match sd with
          | Right | Left -> fail "distributive right or left only"
          | Both ->
            begin
              let arity_dg = Check.get_arity_of_generator dg in
              let arity_eg, eg = (
                match egopt with
                | Some eg -> get_arity_of_generator eg, eg
                | None -> arity_dg, dg
              ) in
              (* Specifications/explanations for the following lines are
                 in the file ocaml_src/parsing/parsetree.mli. *)
              let clauses =
                match arity_eg, arity_dg with
                | Ga_zeroary, Ga_zeroary -> [
                    Clause (
                      list_hd_pattern (pattern dg []),
                      construction0 eg);
                  ]

                | Ga_zeroary, (Ga_unary | Ga_binary | Ga_mary _ | Ga_listary) ->
                  fail "incompatible arities for distributivity"

                | Ga_unary, Ga_unary -> [
                    Clause (
                      list_hd_pattern (pattern dg [x]),
                      construction1 eg
                        (apply_cons g accu (cons x xs)));
                  ]

                | Ga_unary, (Ga_zeroary | Ga_binary | Ga_mary _ | Ga_listary) ->
                  fail "incompatible arities for distributivity"

                | Ga_binary, Ga_binary -> [
                    Clause (
                      list_hd_pattern (pattern dg [x; y]),
                      construction2 eg
                        (apply_cons g accu (cons x xs))
                        (apply_cons g accu (cons y xs)));
                  ]

                | Ga_binary, (Ga_zeroary | Ga_unary | Ga_mary _ | Ga_listary)  ->
                  fail "incompatible arities for distributivity"

                | Ga_mary m, Ga_mary n when m = n ->
                  nyimpl_listary "distributive with mary (m > 2) generators"
                | Ga_mary _, (Ga_zeroary | Ga_unary | Ga_binary | Ga_mary _ | Ga_listary) ->
                  fail "incompatible arities for distributivity"

                | Ga_listary, Ga_listary -> [
                    Clause (
                      list_hd_pattern (pattern dg [x]),
                      construction1 eg (
                        list_map (apply2 (distrib_in g) accu xs) x
                      ));
                  ]

                | Ga_listary, Ga_zeroary -> [
                    Clause (
                      list_hd_pattern (pattern dg []),
                      construction1 eg nil);
                  ]

                | Ga_listary, Ga_unary -> [
                    Clause (
                      list_hd_pattern (pattern dg [x]),
                      construction1 eg (
                        singleton (apply_cons g accu (cons x xs))
                      ));
                  ]

                | Ga_listary, Ga_binary -> [
                    Clause (
                      list_hd_pattern (pattern dg [x; y]),
                      construction1 eg (
                        cons (apply_cons g accu (cons x xs))
                             (singleton (apply_cons g accu (cons y xs)))
                      ));
                   ]
                | Ga_listary, Ga_mary _ ->
                  nyimpl_listary "distributive with listary \
                    generators and optional non-listary generators" in
              clauses
            end
        end

      | Absorbent (sd, ag) ->
          begin
            let right_side =
              match sd with
              | Both ->  return (singleton (construction0 ag))
              | Right -> return rev_accu
              | Left ->  apply_cons g nil l in
            [ Clause (list_hd_underscore (generator0 ag), right_side); ]
          end
      | Rewrite (pat, expr) -> [ Genr_expression.genr_rule pat expr ]
      | Commutative _
      | Structure_item _ | Precedence _ |  Status _ -> []

        (* These relations do not make sense in listary generators
           as specified in parsetree.mli. *)
      | Division_by_Absorbent _ -> fail "listary division by absorbent"
      | Absorbing _ -> fail "listary absorbing"
      | Involutive -> fail "listary involutive" in

    List.map (fun clause -> line, filename, clause) cls_of_ap in

  let default_clauses =
    added_clauses [
       Clause (nil, apply1 (return_in g) rev_accu);
        Clause (
          cons x xs,
          match Relation.inverse relations with
              | Some (_, ginv, _) ->
          delete_call g (Genr_base.construction1 ginv x) x (apply_cons g ys xs)
              | None ->
          apply_cons g (apply_insert g x accu) xs
        )
    ] in

  let clauses_by_ap =
    List.flatten (List.map (genr_clause_by_ap g) relation_list) in
  let clauses = clauses_by_ap @ default_clauses in

  let traced_clauses = trace_clauses clauses in

  let body = Match (l, traced_clauses) in

  Construction_helper (cons_ident, args, body)
;;

(* Delete functions are generated iff an inverse is found *)

let genr_delete g e relations =
  let delete_ident = delete_in g
  and _lt, eq, _gt =
    genr_comparisons (Relation.commutative relations)
  and args = [ x; l; ] in

  (* Got to search for deletion in the entire list if the generator is
     commutative as every generator is virtually (ie "equationally") anywhere.
     In the non-commutative case, just see if the nearest neighbor is a
     candidate for annihilation. *)
  let non_nil_default_clause =
    if has_commutative relations
    then Clause (cons y ys, cons y (apply_delete g x ys))
    else Clause (underscore, raise_error "Not_found" ) in

  let default_clauses = [
    Clause (nil, raise_error "Not_found");
    Clause_when
      (cons y ys, eq x y,
       apply_insert g (construction0 e) ys);
    non_nil_default_clause;
  ] in

  let body =  Match (l, default_clauses) in

  Construction_helper (delete_ident, args, body)
;;

let genr_flat g =
  let flat_ident = flat_in g in
  let clauses = [
    Clause (Genr_base.pattern g [x], x);
    Clause (x, cons x nil);
  ] in

  let body = Function clauses in

  Construction_helper (flat_ident, [], body)
;;

let genr_distrib g =
  let distrib_ident = distrib_in g in
  let args = [ accu; l; x; ] in
  let body =
    construction1 g (append rev_accu (cons x l)) in

  Construction_helper (distrib_ident, args, body)
;;

let genr_return_clauses g relations =
  let neutral = has_neutral relations in
  let neutral_gen () =
    match Relation.neutral relations with
    | None -> assert false
    | Some (_sd, ng) -> construction0 ng in

  let default_clauses = [
    Clause (cons x nil, x);
    Clause (l, generator1 g l);
  ] in

  let clauses =
    lazy_opt neutral
      (fun () -> Clause (nil, neutral_gen ()))
      default_clauses in

  clauses
;;

let genr_return g relations =
  let return_ident = return_in g in

  let body = Function (genr_return_clauses g relations) in

  Construction_helper (return_ident, [], body)
;;

let genr_normalize g _relations =
  let normalize_ident = normalize_in g in
  let args = [ l; ] in
  let nil_clause = Clause (nil, apply1 (return_in g) l) in
  let non_nil_clause = Clause (l, apply2 (cons_in g) nil l) in
  let clauses = [ nil_clause; non_nil_clause; ] in

  let body = Match (l, clauses) in

  Construction_helper (normalize_ident, args , body)
;;

let genr_construction_function g relations =
  let args = [ l; ] in
  let associative = has_associative relations
  and commutative = has_commutative relations in

  (* Pre-treatment of the argument list depending on associativity and
     commutativity of generator g. *)
  let pre_normalize =
    let rev_flat_map cont =
      Let (
        ident_rfm, [f; l; accu],
        Var (Lident " \
          List.rev_append moca_accu\
        \n       (List.fold_left\
        \n          (fun ll l -> List.rev_append (f l) ll)\
        \n          []\
        \n          l)"
        ),
        cont) in

    let cmp = Genr_base.comparison_function (Relation.commutative relations) in

    match associative, commutative with
    | true, true ->
      let ac_pre_normalize code =
        rev_flat_map (
          Let (
            ident_l, [],
            list_sort cmp
              (apply3 ident_rfm (Constant (flat_in g)) l nil),
            code)) in
      ac_pre_normalize

    | true, false ->
      let associative_pre_normalize code =
        rev_flat_map (
          Let (
            ident_l, [],
            list_rev
              (apply3 ident_rfm (Constant (flat_in g))l nil),
            code)) in
      associative_pre_normalize

    | false, true ->
      let commutative_pre_normalize code =
        Let (ident_l, [], list_sort cmp l, code) in
      commutative_pre_normalize

    | false, false ->
      let no_pre_normalize code = code in no_pre_normalize
  in

  let body = (pre_normalize (apply1 (normalize_in g) l)) in

  Construction (g, args, body)
;;

let should_genr_distrib_helper relations =
  let distrib_relations = Relation.distributive relations in
  let rec generate_helper rels =
    match rels with
      | (_, dg, None , _) :: rs ->
        (Check.get_arity_of_generator dg = Ga_listary)
        || generate_helper rs
      | (_, dg, Some eg , _) :: rs ->
        let darity = get_arity_of_generator dg
        and earity = get_arity_of_generator eg in
        (darity = Ga_listary && earity = Ga_listary)
        || generate_helper rs
      | [] -> false
  in generate_helper distrib_relations
;;


let genr_function cdef =

  let g = generator_of_cdef cdef
  and relations = relations_of_cdef cdef in

  let construction_function = genr_construction_function g relations
  and aux_functions = ref [ ] in

  (if should_genr_distrib_helper relations
   then aux_functions := genr_distrib g :: !aux_functions);

  (if (has_idempotent relations || has_nilpotent relations) &&
      not (has_commutative relations)
   then aux_functions := !aux_functions @ genr_potent g relations);

  (if has_associative relations
   then aux_functions := genr_flat g :: !aux_functions) ;
  let insert_functions =
    [genr_return g relations;
     genr_normalize g relations;
     genr_cons g relations;
     genr_insert g relations]

  and delete_functions =
    match Relation.inverse relations, Relation.neutral relations with
    | Some (_sd_inverse, _ginv, Some e), _
    | Some (_sd_inverse, _ginv, None), Some (_, e) ->
      [ genr_delete g e relations; ]
    | _ -> [] in

  construction_function ::
    !aux_functions @ insert_functions @ delete_functions
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
