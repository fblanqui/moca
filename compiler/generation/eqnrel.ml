(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: eqnrel.ml,v 1.42 2012-04-02 09:27:25 weis Exp $ *)

open Parsetree
open Code
open Term
open Term_utils
open Equation
open Useful
open Longident
open Var
open Symb
open Rule
open Otype
open Configuration
;;

exception Not_algebraic
;;

(** Creates a list of patterns from a pattern with or's. *)

let get_tuple p =
  match p.ppat_desc with
  | Ppat_tuple ps -> ps
  | Ppat_any | Ppat_var _ | Ppat_alias _ | Ppat_constant _
  | Ppat_construct _ | Ppat_variant _ | Ppat_record _ | Ppat_array _
  | Ppat_or _ | Ppat_constraint _ | Ppat_type _ | Ppat_lazy _ | Ppat_parens _
    -> assert false
;;

let rec remove_or p =
  let loc d = { p with ppat_desc = d } in
  match p.ppat_desc with
  | Ppat_any
  | Ppat_construct (_, None, _)
  | Ppat_lazy _
  | Ppat_parens _
  | Ppat_tuple []
  | Ppat_constant _
  | Ppat_var _ -> [p]

  | Ppat_alias (q, s) ->
      List.map (fun r -> loc (Ppat_alias (r, s))) (remove_or q)
  | Ppat_construct (lid, Some q, b) ->
      List.map (fun r -> loc (Ppat_construct (lid, Some r, b))) (remove_or q)
  | Ppat_constraint (q, ct) ->
      List.map (fun r -> loc (Ppat_constraint (r, ct))) (remove_or q)

  | Ppat_or (q, r) -> remove_or q @ remove_or r

  | Ppat_tuple (q :: qs) ->
      let ql = remove_or q
      and qsl = List.map get_tuple (remove_or (loc (Ppat_tuple qs))) in
      let f qi = List.map (fun qsj -> loc (Ppat_tuple (qi :: qsj))) qsl in
      List.flatten (List.map f ql)

  | Ppat_variant _
  | Ppat_record _
  | Ppat_array _
  | Ppat_type _ -> raise Not_algebraic
;;

(** Convert an OCaml constant into a term. *)

let term_of_constant c =
  App (symbol_of_string (Pr_ocaml.string_of_constant c), [])
;;

(** Convert an OCaml pattern into a term. *)
(* p must contain no ``or'' pattern *)

type pat_var =
  | PVar of var
  | PAlias of term
;;

let init, fresh =
  let v = ref 0 in
    (fun () -> v := 0),
    (fun () -> incr v; !v)
;;

let term_of_pattern =

  let init_smap, get_smap, var, alias =
    let smap = ref StrMap.empty in
    (fun () -> init (); smap := StrMap.empty),
    (fun () -> !smap),
    (fun s -> let k = fresh () in smap := StrMap.add s (PVar k) !smap; Var k),
    (fun s t -> smap := StrMap.add s (PAlias t) !smap; t) in

  let rec term_of_pat p =
    match p.ppat_desc with
    | Ppat_any -> Var (fresh ())
    | Ppat_var s -> var s
    | Ppat_alias (p, s) -> alias s (term_of_pat p)
    | Ppat_construct (lid, None, _b) -> iapp0 lid
    | Ppat_construct (lid, Some p, _b) -> iapp lid (terms_of_pat p)
    | Ppat_constraint (p, _ct) -> term_of_pat p
    | Ppat_lazy p -> term_of_pat p
    | Ppat_parens p -> term_of_pat p
    | Ppat_constant c -> term_of_constant c
    | Ppat_or _ -> assert false
    | Ppat_tuple _
    | Ppat_type _
    | Ppat_variant _
    | Ppat_record _
    | Ppat_array _ -> raise Not_algebraic

  and terms_of_pat p =
    match p.ppat_desc with
    | Ppat_any
    | Ppat_var _
    | Ppat_alias _
    | Ppat_construct _
    | Ppat_constraint _ -> [term_of_pat p]
    | Ppat_lazy p -> terms_of_pat p
    | Ppat_parens p -> terms_of_pat p
    | Ppat_tuple ps -> List.map term_of_pat ps
    | Ppat_constant c -> [term_of_constant c]
    | Ppat_or _ -> assert false
    | Ppat_type _
    | Ppat_variant _
    | Ppat_record _
    | Ppat_array _ -> raise Not_algebraic in

  fun p ->
    init_smap ();
    let t = term_of_pat p in
    let smap = get_smap () in
    t, smap
;;

let term_of_pattern2 smap =

  let find s =
    try match StrMap.find s smap with
      | PVar k -> Var k
      | PAlias t -> t
    with Not_found -> invalid_arg "term_of_pattern2" in

  let rec term_of_pat p =
    match p.ppat_desc with
    | Ppat_any -> Var (fresh ())
    | Ppat_var s -> find s
    | Ppat_alias (_, s) -> find s
    | Ppat_construct (lid, None, _b) -> iapp0 lid
    | Ppat_construct (lid, Some p, _b) -> iapp lid (terms_of_pat p)
    | Ppat_constraint (p, _ct) -> term_of_pat p
    | Ppat_lazy p -> term_of_pat p
    | Ppat_parens p -> term_of_pat p
    | Ppat_constant c -> term_of_constant c
    | Ppat_or _ -> assert false
    | Ppat_tuple _
    | Ppat_type _
    | Ppat_variant _
    | Ppat_record _
    | Ppat_array _ -> raise Not_algebraic

  and terms_of_pat p =
    match p.ppat_desc with
    | Ppat_any
    | Ppat_var _
    | Ppat_alias _
    | Ppat_construct _
    | Ppat_constraint _ -> [term_of_pat p]
    | Ppat_lazy p -> terms_of_pat p
    | Ppat_parens p -> terms_of_pat p
    | Ppat_tuple ps -> List.map term_of_pat ps
    | Ppat_constant c -> [term_of_constant c]
    | Ppat_or _ -> assert false
    | Ppat_type _
    | Ppat_variant _
    | Ppat_record _
    | Ppat_array _ -> raise Not_algebraic in

    term_of_pat
;;

(** Recognize non-linear rules. *)

let rec remove_parens e =
  match e.pexp_desc with
  | Pexp_parens e
  | Pexp_begend e -> remove_parens e
  | x -> x
;;

let is_var_opt smap e =
  match remove_parens e with
    | Pexp_ident (Lident s) ->
        begin
          try
            match StrMap.find s smap with
              | PVar k -> Some (s, k)
              | PAlias _ -> None
          with Not_found -> None
        end
    | _ -> None
;;

let is_eq e =
  match remove_parens e with
    | Pexp_ident (Lident ("=" | "==")) -> true
    | _ -> false
;;

let is_and e =
  match remove_parens e with
    | Pexp_ident (Lident ("&&" | "&")) -> true
    | _ -> false
;;

let is_compare e =
  match remove_parens e with
    | Pexp_ident (Lident "compare" |
                      Ldot (Lident "Pervasives", "compare")) -> true
    | _ -> false
;;

let is_zero e =
  match remove_parens e with
    | Pexp_constant (Asttypes.Const_int 0) -> true
    | _ -> false
;;

let is_app_opt e =
  match remove_parens e with
    | Pexp_apply (e, [_, e1; _, e2]) -> Some (e, e1, e2)
    | _ -> None
;;

(*let exists s1 s2 =
  let f s _ b = b or s = s1 or s = s2 in
    fun smap -> StrMap.fold f smap false
;;*)

exception Found
;;

let exists s1 s2 =
  let f s _ = if s = s1 or s = s2 then raise Found else () in
  fun smap -> try StrMap.iter f smap; false with Found -> true
;;

let rec add_eqn ((s1, k1) as x1) ((s2, k2) as x2) = function
  | [] -> [StrMap.add s1 k1 (StrMap.add s2 k2 StrMap.empty)]
  | sm :: l when exists s1 s2 sm ->
      StrMap.add s1 k1 (StrMap.add s2 k2 sm) :: l
  | sm :: l -> sm :: add_eqn x1 x2 l
;;

let add_var_equality_aux smap acc e1 e2 =
  match is_var_opt smap e1, is_var_opt smap e2 with
    | Some x1, Some x2 -> add_eqn x1 x2 acc
    | _, _ -> raise Not_algebraic
;;

let add_var_equality smap acc e =
  match is_app_opt e with
    | Some (f, u, v) when is_eq f ->
      begin
        match is_app_opt u, is_app_opt v with
          | Some (c, e1, e2), _ when is_compare c && is_zero v ->
            add_var_equality_aux smap acc e1 e2
          | _, Some (c, e1, e2) when is_compare c && is_zero u ->
            add_var_equality_aux smap acc e1 e2
          | _ -> add_var_equality_aux smap acc u v
      end
    | _ -> raise Not_algebraic
;;

let rec add_var_equalities smap acc e =
  match remove_parens e with
    | Pexp_apply (e, [_, e1; _, e2]) when is_and e ->
      add_var_equalities smap (add_var_equalities smap acc e1) e2
    | _ -> add_var_equality smap acc e
;;

let var_equalities smap =
  let rec aux acc e =
    match remove_parens e with
      | Pexp_when (e1, e2) -> aux (add_var_equalities smap acc e1) e2
      | _ -> acc, e
  in aux []
;;

(*let choose =
  let f _s k _x = k in
    fun sm -> StrMap.fold f sm 0
;;*)

exception Found of int
;;

let choose =
  let f _ k = raise (Found k) in
    fun sm -> try StrMap.iter f sm; assert false with Found k -> k
;;

let build_maps_from_var_equalities =
  let rec aux acc = function
    | [] -> acc
    | sm :: l ->
        let k0 = choose sm in
        let f s k (smap, imap) =
          StrMap.add s (PVar k0) smap,
          VarMap.add k k0 imap
        in aux (StrMap.fold f sm acc) l
  in aux (StrMap.empty, VarMap.empty)
;;

(** Convert an OCaml expression into a term. *)

let term_of_lid smap = function
  | Lident s as lid ->
      begin
        try
          match StrMap.find s smap with
            | PVar k -> Var k
            | PAlias t -> t
        with Not_found -> iapp0 lid
      end
  | Ldot _ | Lapply _ as lid -> iapp0 lid
;;

let rec term_of_exp smap e =
  match e.pexp_desc with
  | Pexp_ident lid -> term_of_lid smap lid
  | Pexp_construct (lid, None, _b) -> iapp0 lid
  | Pexp_construct (lid, Some e, _b) -> iapp lid (terms_of_exp smap e)
  | Pexp_parens e
  | Pexp_begend e -> term_of_exp smap e
  | Pexp_constant c -> term_of_constant c
  (* non algebraic expressions *)
  | Pexp_tuple _ | Pexp_let _ | Pexp_function _ | Pexp_apply _
  | Pexp_match _ | Pexp_try _ | Pexp_variant _ | Pexp_record _
  | Pexp_field _ | Pexp_setfield _ | Pexp_array _ | Pexp_ifthenelse _
  | Pexp_sequence _ | Pexp_while _ | Pexp_for _ | Pexp_constraint _
  | Pexp_when _ | Pexp_send _ | Pexp_new _ | Pexp_setinstvar _
  | Pexp_override _ | Pexp_letmodule _ | Pexp_assert _
  | Pexp_assertfalse | Pexp_lazy _
  | Pexp_poly _ | Pexp_object _
  | Pexp_open (_, _)
  | Pexp_pack (_, _)
  | Pexp_newtype (_, _) -> raise Not_algebraic


and terms_of_exp smap e =
  match e.pexp_desc with
  | Pexp_ident _
  | Pexp_construct _
  | Pexp_begend _ -> [term_of_exp smap e]
  | Pexp_parens e -> terms_of_exp smap e
  | Pexp_tuple es -> List.map (term_of_exp smap) es
  | Pexp_constant c -> [term_of_constant c]
  (* non algebraic expressions *)
  | Pexp_let _ | Pexp_function _ | Pexp_apply _
  | Pexp_match _ | Pexp_try _ | Pexp_variant _ | Pexp_record _
  | Pexp_field _ | Pexp_setfield _ | Pexp_array _ | Pexp_ifthenelse _
  | Pexp_sequence _ | Pexp_while _ | Pexp_for _ | Pexp_constraint _
  | Pexp_when _ | Pexp_send _ | Pexp_new _ | Pexp_setinstvar _
  | Pexp_override _ | Pexp_letmodule _ | Pexp_assert _
  | Pexp_assertfalse | Pexp_lazy _
  | Pexp_poly _ | Pexp_object _
  | Pexp_open (_, _)
  | Pexp_pack (_, _)
  | Pexp_newtype (_, _) -> raise Not_algebraic
;;

(** Convert a Moca relation into a set of equations. *)

let rules_of_rewrite p e =
  let ps = remove_or p in
  let imap, l, smap, e, ps =
    match ps with
      | [] -> assert false
      | p :: ps ->
        let l, smap = term_of_pattern p in
        let eqs, e = var_equalities smap e in
        let _smap, imap = build_maps_from_var_equalities eqs in
        imap, l, smap, e, ps in
(*  Format.printf "SMAP ====@.";
  StrMap.iter
    (fun s v ->
      Format.fprintf Format.std_formatter "S %s ------> %a@."
        s pr_pat_var v) smap;
  VarMap.iter
    (fun v i->
      Format.fprintf Format.std_formatter "I %a ------> %d@."
        Var.pr_var v i ) imap;
  *)
  let r = term_of_exp smap e in
  let f p =
    let p'= term_of_pattern2 smap p in
    let r = term_of_exp smap e in
(*    Format.fprintf Format.std_formatter "Rules_rewrite: %a ==> %a@."
      Term.pr_term p'
      Term.pr_term r; *)
    p', r in
  let p' = rename imap l in
(*  Format.fprintf Format.std_formatter "Rules_rewrite: %a ==> %a@."
    Term.pr_term p'
    Term.pr_term r; *)
  (p', r) :: List.map f ps
;;

let not_algebraic = ref false;;

let eqns_of_rel c rel =
  match rel.prel_desc with
  | Rewrite (p, e) ->
      (try rules_of_rewrite p e
       with Not_algebraic -> not_algebraic := true; [])

  | Commutative _cmp -> [app2 c x y, app2 c y x]
  | Associative _sd -> [app2 c (app2 c x y) z, app2 c x (app2 c y z)]

  | Absorbent (Left, d) -> [app2 c (app0 d) x, app0 d]
  | Absorbent (Right, d) -> [app2 c x (app0 d), app0 d]
  | Absorbent (Both, d) when Check.arity_of_generator c = 1 ->
      [app1 c (app0 d), (app0 d)]
  | Absorbent (Both, d) ->
      [app2 c (app0 d) x, (app0 d); app2 c x (app0 d), app0 d]

  | Absorbing (Left, d) -> [app2 c (app2 d x y) y, y]
  | Absorbing (Right, d) -> [app2 c x (app2 d x y), x]
  | Absorbing (Both, d) ->
      [app2 c (app2 d x y) y, y; app2 c x (app2 d x y), x]

  | Neutral (Left, e) -> [app2 c (app0 e) x, x]
  | Neutral (Right, e) -> [app2 c x (app0 e), x]
  | Neutral (Both, e) ->
      [app2 c (app0 e) x, x; app2 c x (app0 e), x]

  | Idempotent Both when Check.arity_of_generator c = 1 ->
      [app1 c (app1 c x), app1 c x]
  | Idempotent Left -> [app2 c (app2 c x y) y, app2 c x y]
  | Idempotent Right -> [app2 c x (app2 c x y), app2 c x y]
  | Idempotent Both ->
      [ app2 c (app2 c x y) y, app2 c x y;
        app2 c x (app2 c x y), app2 c x y; ]

  | Nilpotent (Both, a) when Check.arity_of_generator c = 1 ->
      [app1 c (app1 c x), app0 a]
  | Nilpotent (Left, a) -> [app2 c (app2 c x y) y, app2 c x (app0 a)]
  | Nilpotent (Right, a) -> [app2 c x (app2 c x y), app2 c (app0 a) y]
  | Nilpotent (Both, a) ->
      [ app2 c x (app2 c x y), app2 c (app0 a) y;
        app2 c (app2 c x y) y, app2 c x (app0 a); ]

  | Involutive -> [app1 c (app1 c x), x]

  (* Inverse for unary generators *)
  | Inverse (Left, i, None) when Check.arity_of_generator c = 1 ->
      [ app1 c (app1 i x), x; ]
  | Inverse (Right, i, None) when Check.arity_of_generator c = 1 ->
      [ app1 i (app1 c x), x; ]
  | Inverse (Both, i, None) when Check.arity_of_generator c = 1 ->
      [ app1 c (app1 i x), x;
        app1 i (app1 c x), x; ]

  | Inverse (Left, i, Some a) when Check.arity_of_generator c = 1 ->
      [ app1 c (app1 i x), app0 a; ]
  | Inverse (Right, i, Some a) when Check.arity_of_generator c = 1 ->
      [ app1 i (app1 c x), app0 a; ]
  | Inverse (Both, i, Some a) when Check.arity_of_generator c = 1 ->
      [ app1 c (app1 i x), app0 a;
        app1 i (app1 c x), app0 a; ]

  (* Inverse for binary generators *)
  | Inverse (Left, i, None) ->
      let e = Check.neutral_element_of_generator c in
      [app2 c (app1 i x) x, app0 e]
  | Inverse (Right, i, None) ->
      let e = Check.neutral_element_of_generator c in
      [app2 c x (app1 i x), app0 e]
  | Inverse (Both, i, None) ->
      let e = Check.neutral_element_of_generator c in
      [app2 c (app1 i x) x, app0 e; app2 c x (app1 i x), app0 e]

  | Inverse (Left, i, Some a) -> [app2 c (app1 i x) x, app0 a]
  | Inverse (Right, i, Some a) -> [app2 c x (app1 i x), app0 a]
  | Inverse (Both, i, Some a) ->
      [app2 c (app1 i x) x, app0 a; app2 c x (app1 i x), app0 a]

  | Distributive (_, d, gopt, b) when Check.arity_of_generator c = 1 ->
      let e =
        match gopt with
        | Some e -> e
        | None -> d
      and n = Check.arity_of_generator d in
      let xs = vars n in
      let args = List.map (app1 c) xs in
      let args =
        match b with
        | Dist_Inverse -> List.rev args
        | Dist_Direct -> args in
      [app1 c (app d xs), app e args]

  | Distributive (Left, d, gopt, b) ->
      let e =
        match gopt with
        | Some e -> e
        | None -> d
      and n = Check.arity_of_generator d in
      let xs = vars n in
      let args = List.map (fun x -> app2 c x z) xs in
      let args =
        match b with
        | Dist_Inverse -> List.rev args
        | Dist_Direct -> args in
      [app2 c (app d xs) z, app e args]

  | Distributive (Right, d, gopt, b) ->
      let e =
        match gopt with
        | Some e -> e
        | None -> d
      and n = Check.arity_of_generator d in
      let xs = vars n in
      let args = List.map (fun x -> app2 c z x) xs in
      let args =
        match b with
        | Dist_Inverse -> List.rev args
        | Dist_Direct -> args in
      [app2 c z (app d xs), app e args]

  | Distributive (Both, d, gopt, b) ->
      let e =
        match gopt with
        | None -> d
        | Some e -> e
      and n = Check.arity_of_generator d in
      let xs = vars n in
      let args1 = List.map (fun x -> app2 c x z) xs
      and args2 = List.map (fun x -> app2 c z x) xs in
      let args1, args2 =
        match b with
        | Dist_Inverse -> List.rev args1, List.rev args2
        | Dist_Direct -> args1, args2 in
      [app2 c (app d xs) z, app e args1;
       app2 c z (app d xs), app e args2]

  | Division_by_Absorbent _
  | Structure_item _
  | Status _
  | Precedence _ -> []
;;

let eqnset_of_rels c rels =
  match rels.prels_desc with
  | Prels_none
  | Prels_commented _ -> EqnSet.empty
  | Prels_begend rs ->
    EqnSet.union_map
      (fun r -> EqnSet.of_list (List.map Equation.mk (eqns_of_rel c r))) rs
;;

let eqnset_of_cdef (cname, _cts, rels, _loc) =
  let c = { pgen_desc = Lident cname; pgen_loc = Location.none } in
  eqnset_of_rels c rels
;;

let eqnset_of_cdefs = EqnSet.union_map eqnset_of_cdef
;;

let eqnset_of_ntd (_tname, td) =
  match td.ptype_kind with
  | Ptype_variant cdefs -> eqnset_of_cdefs cdefs
  | Ptype_record _
  | Ptype_abstract _ -> EqnSet.empty
;;

let eqnset_of_ntds = EqnSet.union_map eqnset_of_ntd
;;

(** Convert a rule into a Moca relation. *)

let lid_of_symb f = Longident.parse (string_of_symbol f)
;;

let rec opat_of_term t = mk_pat (opat_of_term_aux t)

and opat_of_terms = function
  | [] | [_] -> assert false
  | ts -> mk_pat (Ppat_tuple (List.map opat_of_term ts))

and opat_of_term_aux = function
  | Var k -> Ppat_var (string_of_var k)
  | App (f, []) -> Ppat_construct (lid_of_symb f, None, true)
  | App (f, [t]) ->
      Ppat_construct (lid_of_symb f, Some (opat_of_term t), true)
  | App (f, ts) ->
      Ppat_construct (lid_of_symb f, Some (opat_of_terms ts), true)
;;

let rec oexp_of_term t = {
  pexp_desc = oexp_of_term_aux t;
  pexp_loc = Location.none;
}

and oexp_of_terms ts = {
   pexp_desc = Pexp_tuple (List.map oexp_of_term ts);
   pexp_loc = Location.none;
}

and oexp_of_term_aux = function
  | Var k -> Pexp_ident (Lident (string_of_var k))
  | App (f, []) -> Pexp_construct (lid_of_symb f, None, true)
  | App (f, ts) ->
      Pexp_construct (lid_of_symb f, Some (oexp_of_terms ts), true)
;;

let gen_of_symb f = {
  pgen_desc = lid_of_symb f;
  pgen_loc = Location.none;
}
;;

let gen_of_symb_opt d e = if d = e then None else Some (gen_of_symb e)
;;

let check_dist_unary c =
  let rec aux = function
    | Var x :: xs, App (c', [Var x']) :: xsz
      when x = x' && c = c' ->
      aux (xs, xsz)
    | [], [] -> true
    | _, _ -> false
  in fun ((xs, _xsz) as l) ->
    VarSet.cardinal (vars_of_terms xs) = List.length xs
      && aux l
;;

let check_dist_binary c z =
  let rec aux = function
    | Var x :: xs, App (c', [Var x'; Var z']) :: cxsz
      when x = x' && c = c' && z = z' ->
      aux (xs, cxsz)
    | [], [] -> true
    | _, _ -> false
  in fun ((xs, _cxsz) as l) ->
    let vars = vars_of_terms xs in
      VarSet.cardinal vars = List.length xs
        && not (VarSet.mem z vars)
        && aux l
;;

let exp_of_var k = mk_ident (string_of_var k)
;;

let exp_eq () =
  mk_ident (if Genr_base.get_sharable_target () then "==" else "=")
;;

let exp_eq_test x y = mk_apply (exp_eq ()) [exp_of_var x; exp_of_var y]
;;

let exp_true = mk_ident "true"
;;

let oexp_desc_of_rule l r =
  let l, imap = linearize l in
  let f x y e =
    if e == exp_true then exp_eq_test x y
    else mk_apply (mk_ident "&&") [exp_eq_test x y; e] in
  let e =
    if VarMap.is_empty imap then oexp_of_term r
    else mk_when (VarMap.fold f imap exp_true) (oexp_of_term r)
  in Rewrite (opat_of_term l, e)
;;

let rel_desc_of_rule = function
  (* Commutative *)
  | App (c1, [Var x1; Var y1]), App (c2, [Var x2; Var y2])
    when c1 = c2 && x1 = x2 && y1 = y2 && x1 <> y1 ->
      c1, Commutative None

  (* Associative *)
  | App (c1, [App (c2, [Var x1; Var y1]); Var z1]),
      App (c3, [Var x2; App (c4, [Var y2; Var z2])])
    when c1 = c2 && c2 = c3 && c3 = c4 && x1 = x2 && y1 = y2 && z1 = z2
      && x1 <> y1 && x1 <> z1 && y1 <> z1 ->
      c1, Associative Left
  | App (c1, [Var x1; App (c2, [Var y1; Var z1])]),
      App (c3, [App (c4, [Var x2; Var y2]); Var z2])
    when c1 = c2 && c2 = c3 && c3 = c4 && x1 = x2 && y1 = y2 && z1 = z2
      && x1 <> y1 && x1 <> z1 && y1 <> z1 ->
      c1, Associative Right

  (* Absorbent *)
  | App (c, [App (d1, []); Var _x1]), App (d2, [])
    when d1 = d2 ->
      c, Absorbent (Left, gen_of_symb d1)
  | App (c, [Var _x1; App (d1, [])]), App (d2, [])
    when d1 = d2 ->
      c, Absorbent (Right, gen_of_symb d1)
  | App (c, [App (d1, [])]), App (d2, [])
    when d1 = d2 ->
      c, Absorbent (Both, gen_of_symb d1)

  (* Absorbing *)
  | App (c, [App (d, [Var x1; Var y1]); Var y2]), Var y3
    when y1 = y2 && y2 = y3 && x1 <> y1 ->
      c, Absorbing (Left, gen_of_symb d)
  | App (c, [Var x1; App (d, [Var x2; Var y1])]), Var x3
    when x1 = x2 && x2 = x3 && x1 <> y1 ->
      c, Absorbing (Right, gen_of_symb d)

  (* Neutral *)
  | App (c, [App (e, []); Var x1]), Var x2
    when x1 = x2 ->
      c, Neutral (Left, gen_of_symb e)
  | App (c, [Var x1; App (e, [])]), Var x2
    when x1 = x2 ->
      c, Neutral (Right, gen_of_symb e)

  (* Idempotent *)
  (* Idempotent for unary generator *)
  | App (c1, [App (c2, [Var x1])]),
      App (c3, [Var x2])
    when c1 = c2 && c2 = c3 && x1 = x2 ->
      c1, Idempotent Both
  (* Idempotent for binary generator *)
  | App (c1, [App (c2, [Var x1; Var y1]); Var y2]),
      App (c3, [Var x2; Var y3])
    when c1 = c2 && c2 = c3 && x1 = x2 && y1 = y2 && y2 = y3 && x1 <> y1 ->
      c1, Idempotent Left
  | App (c1, [Var x1; App (c2, [Var x2; Var y1])]),
      App (c3, [Var x3; Var y2])
    when c1 = c2 && c2 = c3 && x1 = x2 && x2 = x3 && y1 = y2 && x1 <> y1 ->
      c1, Idempotent Right
  | App (c, [Var x1; Var x2]), Var x3
    when x1 = x2 && x2 = x3 ->
      c, Idempotent Both

  (* Nilpotent *)
  (* Nilpotent for unary generator *)
  | App (c1, [App (c2, [Var _x])]),
      App (a, [])
    when c1 = c2 ->
      c1, Nilpotent (Both, gen_of_symb a)
  (* Nilpotent for binary generator *)
  | App (c1, [App (c2, [Var x1; Var y1]); Var y2]),
      App (c3, [Var x2; App (a, [])])
    when c1 = c2 && c2 = c3 && x1 = x2 && y1 = y2 && x1 <> y1 ->
      c1, Nilpotent (Left, gen_of_symb a)
  | App (c1, [Var x1; App (c2, [Var x2; Var y1])]),
      App (c3, [App (a, []); Var y2])
    when c1 = c2 && c2 = c3 && x1 = x2 && y1 = y2 && x1 <> y1 ->
      c1, Nilpotent (Right, gen_of_symb a)
  | App (c, [Var x1; Var x2]), App (a, [])
    when x1 = x2 ->
      c, Nilpotent (Both, gen_of_symb a)

  (* Involutive *)
  | App (c1, [App (c2, [Var x1])]), Var x2
    when c1 = c2 && x1 = x2 ->
      c1, Involutive

  (* Inverse *)
  (* Inverse for unary generator *)
  | App (c, [App (i, [Var x1])]), Var x2
    when x1 = x2 ->
      c, Inverse (Left, gen_of_symb i, None)
  (* Can not handle the Right side, as it has the same pattern
  | App (i, [App (c, [Var x1])]), Var x2
    when x1 = x2 ->
      c, Inverse (Right, gen_of_symb i, None)
  *)
  | App (c, [App (i, [Var _x1])]), App (a, []) ->
      c, Inverse (Left, gen_of_symb i, Some (gen_of_symb a))
  (* Can not handle the Right side, as it has the same pattern
  | App (i, [App (c, [Var _x1])]), App (a, [])
    when x1 = x2 ->
      c, Inverse (Right, gen_of_symb i, Some (gen_of_symb a))
  *)

  (* Inverse for binary generator *)
  | App (c, [App (i, [Var x1]); Var x2]), App (a, [])
    when x1 = x2 ->
      c, Inverse (Left, gen_of_symb i, Some (gen_of_symb a))
  | App (c, [Var x1; App (i, [Var x2])]), App (a, [])
    when x1 = x2 ->
      c, Inverse (Right, gen_of_symb i, Some (gen_of_symb a))

  (* Distributive for a unary constructor [c] *)
  | App (c, [App (d, xs)]), App (e, cxs)
      when check_dist_unary c (xs, cxs) ->
      c, Distributive (Both, gen_of_symb d, gen_of_symb_opt d e,
               Dist_Direct)
  | App (c, [App (d, xs)]), App (e, cxs)
      when check_dist_unary c (List.rev xs, cxs) ->
      c, Distributive (Both, gen_of_symb d, gen_of_symb_opt d e,
               Dist_Inverse)
  (* Distributive for a binary constructor [c] *)
  | App (c, [App (d, xs); Var z]), App (e, cxsz)
    when check_dist_binary c z (xs, cxsz) ->
      c, Distributive (Left, gen_of_symb d, gen_of_symb_opt d e,
               Dist_Direct)
  | App (c, [App (d, xs); Var z]), App (e, cxsz)
    when check_dist_binary c z (List.rev xs, cxsz) ->
      c, Distributive (Left, gen_of_symb d, gen_of_symb_opt d e,
               Dist_Inverse)
  (* Rewrite *)
  | App (f, _) as l, r ->
      f, oexp_desc_of_rule l r
  | Var _, _ -> assert false
;;

let relation_of_rule (Rule.Mk (l,r)) =
  let g, rd = rel_desc_of_rule (l,r) in
  g, { prel_desc = rd; prel_loc = Location.none }
;;

let add_rule r smap =
  let f, r = relation_of_rule r in
  try let rs = SymbolMap.find f smap in
      SymbolMap.add f (r :: rs) smap with
  | Not_found -> SymbolMap.add f [r] smap
;;

let cd_of_rule smap (s, cts, rels, loc) =
  let f = symbol_of_string s in
  let rs = try SymbolMap.find f smap with Not_found -> [] in
    s, cts, Otype.norm (Otype.add_rules rs (Otype.remove_rules rels)), loc
;;

let td_of_rules smap td =
  match td.ptype_kind with
  | Ptype_variant cdefs ->
      { td with ptype_kind = Ptype_variant (List.map (cd_of_rule smap) cdefs) }
  | Ptype_abstract _ | Ptype_record _ -> td
;;

let ntd_of_rules smap (n, td) = n, td_of_rules smap td
;;

let ntds_of_rules rs =
  let smap = RulSet.fold add_rule rs SymbolMap.empty in
  List.map (ntd_of_rules smap)
;;

(** Build the precedence and the status map from the declarations. *)

let insert ((_c1, k1) as x1) =
  let rec aux = function
    | [] -> [x1]
    | (_c2, k2) as x2 :: l when k1 > k2 -> x2 :: aux l
    | l -> x1 :: l in
  aux
;;

let add_relation ((prec, stat) as cmap) cname r =
  match r.prel_desc with
  | Precedence k -> insert (symbol_of_string cname, k) prec, stat
  | Status Multiset
  | Commutative _ ->
      prec, SymbolMap.add (symbol_of_string cname) Multiset stat
  | Status Lexicographic | Rewrite _ | Associative _
  | Absorbent _ | Absorbing _ | Neutral _ | Idempotent _
  | Nilpotent _ | Involutive | Inverse _ | Distributive _
  | Division_by_Absorbent _ | Structure_item _ ->
      cmap (* default status is Lexicographic *)
;;

let rec add_relation_list cmap cname = function
  | [] -> cmap
  | r :: rs -> add_relation_list (add_relation cmap cname r) cname rs
;;

let add_relations cmap cname rels =
  match rels.prels_desc with
  | Prels_none
  | Prels_commented _ -> cmap
  | Prels_begend rs -> add_relation_list cmap cname rs
;;

let rec add_generators cmap = function
  | [] -> cmap
  | (cname, _cts, rels, _loc) :: cds ->
      add_generators (add_relations cmap cname rels) cds
;;

let add_ntd cmap (_tname, td) =
  match td.ptype_kind with
  | Ptype_record _
  | Ptype_abstract _ -> cmap
  | Ptype_variant cdefs -> add_generators cmap cdefs
;;

let rec add_ntds cmap = function
  | [] -> cmap
  | ntd :: ntds -> add_ntds (add_ntd cmap ntd) ntds
;;

let prec_status_of_ntds ntds =
  let prec, stat = add_ntds ([], SymbolMap.empty) ntds in
  Prec.prec_of_list (List.map fst prec), stat
;;

(* Do we use completion ? *)
let get_kb, set_kb =
  let kb = ref false in
  (fun () -> !kb),
  (fun () -> kb := true)
;;

(* Maximal number of completion steps. *)
let get_kb_limit, set_kb_limit =
  let kb_limit = ref 5 in
  (fun () -> !kb_limit),
  (fun n -> kb_limit := n)
;;

(** Completion. *)

open Order;;
open Comp;;
open Rule;;

type kb_result =
  | Success of (string * type_declaration) list
  | Incomplete
  | Fail of EqnSet.t
;;

let completion n ntds =
  not_algebraic := false;
  let prec, stat = prec_status_of_ntds ntds
  and es = eqnset_of_ntds ntds
  and acs = RulSet.empty in
  let cmp = rpo stat (Prec.compare prec) in
  verbose_print
    "Knuth-Bendix completion of:\n%a\n" EqnSet.fprintf es;
  try
    let (es,rs), i = complete_n acs cmp n (es, RulSet.empty) in
    verbose_printf "%s" Debug.sep;
    if EqnSet.is_empty es then
      (verbose_printf "Completed in %i steps\n" i;
       Success (ntds_of_rules rs ntds), !not_algebraic)
    else
      (if i = n then verbose_printf "Incompleted after %i steps\n" i;
       Incomplete, !not_algebraic)
  with
    | Unorientable es ->
        verbose_print "%sUnorientable equations:\n%a\n"
          Debug.sep EqnSet.fprintf es;
        Fail es, !not_algebraic
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
