(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Laura Lowenthal, projet Protheo, INRIA Lorraine           *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: genr_listary_equations.ml,v 1.16 2011-12-05 15:38:54 weis Exp $ *)

(** {Generation of test equations for listary constructors.} *)

open Parsetree
open Term
open Term_utils
open Otype
open Symb
;;

let rec term_of_list = function
  | [] -> App (symbol_of_string "[]", [])
  | t :: ts -> App (symbol_of_string "::", [ t; term_of_list ts; ])
;;

let app_to_list g ts = app1 g (term_of_list ts)
;;

let rec testing_equations_of_rel g rel =
  match rel.prel_desc with
  | Rewrite _ -> Eqnrel.eqns_of_rel g rel
  | Commutative _ ->
    [
     app_to_list g [x; y; z; w], app_to_list g [y; x; w; z];
     app_to_list g [x; y; z; w], app_to_list g [w; y; z; x];
     app_to_list g [x; y; z; w], app_to_list g [w; z; y; x];
     app_to_list g [x; y; z; w], app_to_list g [z; y; x; w];
    ]
  | Associative _ ->
    [
     app_to_list g [app_to_list g [x; y]; z], app_to_list g [x; y; z];
     app_to_list g [x; app_to_list g [y; z]; app_to_list g [w; Var 4]],
     app_to_list g [x; y; z; w; Var 4];
     app_to_list g [app_to_list g [x; y; z]; w], app_to_list g [x; y; z; w];
    ]
  | Absorbent _ -> []
  | Absorbing _ -> []
  | Neutral (Left, e) ->
    let e = app0 e in
    [
     app_to_list g [x; e; y], app_to_list g [x; y];
     app_to_list g [e; x], x;
     app_to_list g [x; e; e], app_to_list g [x; e];
     app_to_list g [x; e; e; y], app_to_list g [x; y];
    ]
  | Neutral (Right, e) ->
    let e = app0 e in
    [
     app_to_list g [x; e; y], app_to_list g [x; y];
     app_to_list g [x; e], x;
     app_to_list g [e; e; y], app_to_list g [e; y];
     app_to_list g [x; e; e; y], app_to_list g [x; y];
    ]
  | Neutral (Both, e) ->
    let e = app0 e in
    [
     app_to_list g [x; e; y], app_to_list g [x; y];
     app_to_list g [e; x], x;
     app_to_list g [x; e; e], x;
    ]
  | Idempotent Left ->
    [
     app_to_list g [x; x; y], app_to_list g [x; y];
     app_to_list g [x; y; y; z], app_to_list g [x; y; z];
    ]
  | Idempotent Right ->
    [
     app_to_list g [y; x; x], app_to_list g [y; x];
     app_to_list g [x; y; y; z], app_to_list g [x; y; z];
    ]
  | Idempotent Both ->
    [
     app_to_list g [x; x], x;
     app_to_list g [x; y; y; z], app_to_list g [x; y; z];
    ]
  | Nilpotent ((Left | Right | Both), e) ->
    let e = app0 e in
    [
     app_to_list g [x; x], e;
     (* If x = y the next rule features a critical pair:
        g [ x; x; x; z] -> g [ e; x; z ] but
        g [ x; e; z ] is not reducible. Unfortunately,
        g [ e; x; z ] is not supposed to be equal to g [ x; e; z ]
        when g is not commutative. *)
     app_to_list g [x; y; y; z], app_to_list g [x; e; z];
    ]
    (* Not applicable for listary (should have been verified before) *)
  | Involutive -> assert false
  | Inverse (_, _, None) -> []
  | Inverse (Left, i, Some e) ->
    let e = app0 e in
    [
     app_to_list g [app1 i x; x], e;
     app_to_list g [x; app1 i y; y; z], app_to_list g [x; e; z];
    ]
  | Inverse (Right, i, Some e) ->
    let e = app0 e in
    [
     app_to_list g [x; app1 i x], e;
     app_to_list g [x; y; app1 i y; z], app_to_list g [x; e; z];
    ]
  | Inverse (Both, i, e) ->
    testing_equations_of_rel g (mk_rel (Inverse (Left, i, e))) @
    testing_equations_of_rel g (mk_rel (Inverse (Right, i, e)))
  | Distributive (_, d, eopt, dist) ->
    let e =
      match eopt with
      | Some e -> e
      | None -> d in
    let d_is_listary = Check.is_listary_generator d in
    let n =
      if d_is_listary then 3 else Check.arity_of_generator d in
    let vs = Term_utils.vars n in
    let args1 = List.map (fun v -> app_to_list g [x; y; v; z]) vs in
    let args2 = List.map (fun v -> app_to_list g [v; x]) vs in
    let args1, args2 =
      match dist with
      | Dist_Inverse -> List.rev args1, List.rev args2
      | Dist_Direct -> args1, args2 in
    let apply g =
      if Check.is_listary_generator g
      then app_to_list g
      else Term_utils.app g in
    [
     app_to_list g [x; y; apply d vs; z], apply e args1;
     app_to_list g [apply d vs; x], apply e args2;
    ]
    (* Not applicable for listary (should have been verified before) *)
  | Division_by_Absorbent _ -> assert false
  | Structure_item _
  | Status _
  | Precedence _ -> []
;;

let testing_equations_of_rels c rels =
  match rels.prels_desc with
  | Prels_none
  | Prels_commented _ -> []
  | Prels_begend rs -> Listutils.flat_map (testing_equations_of_rel c) rs
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
