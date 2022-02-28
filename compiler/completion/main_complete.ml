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

(* $Id: main_complete.ml,v 1.13 2010-09-20 17:48:05 bonichon Exp $ *)

open Comp;;
open Term;;
open Order;;
open Equation;;
open Rule;;
open Useful;;
open Axiom;;
open Prec;;

(************************************************************************)
(* completion *)

let comp max (acs, es, rs, prec) =
  let gt t u = lpo (Prec.compare prec) t u = Greater in
  try
    let ((es,rs) as ers), i = complete_n acs gt max (es,rs) in
    if EqnSet.is_empty es then
      Printf.printf "completed in %i steps\n%a\n"
	i (to_channel RulSet.bprint) rs
    else if i = max then
      Printf.printf "incompleted after %i steps\n%a\n"
	i (to_channel Comp.bprint) ers
    else ()
  with Unorientable es ->
    Printf.printf "found unorientable equations\n%a\n"
      (to_channel EqnSet.bprint) es;;

(************************************************************************)
(* conversion functions *)

let equations_of_lexbuf = Cparser.equations Clexer.next_token;;
let equations_of_string s = equations_of_lexbuf (Lexing.from_string s);;

let rules_of_lexbuf = Cparser.rules Clexer.next_token;;
let rules_of_string s = rules_of_lexbuf (Lexing.from_string s);;

let c_rules fs =
  rulset_of_theory (List.fold_left (fun th f -> Com f :: th) [] fs);;

let ac_rules fs =
  rulset_of_theory (List.fold_left (fun th f -> Ass f :: Com f :: th) [] fs);;

let pb_of_str acs cs es rs prec =
  let es = equations_of_string es and rs = rules_of_string rs
  and acs = RulSet.union (ac_rules acs) (c_rules cs) in
    acs, es, rs, prec;;

let pb_of_theory acs cs es rs prec =
  let es = eqnset_of_theory es and rs = rulset_of_theory rs
  and acs = RulSet.union (ac_rules acs) (c_rules cs) in
    acs, es, rs, prec;;

(************************************************************************)
(* a precedence *)

let symb_list = List.map Symb.symbol_of_string ["0"; "1"; "+"; "-"; "*";
 "True"; "False"; "And"; "Or"; "Not"]

let prec = prec_of_list symb_list;;

let ac_pb acs cs es = pb_of_theory acs cs es [] prec;;

let pb = ac_pb [] [];;

let ac_pb_str acs cs es = pb_of_str acs cs es "" prec;;

let pb_str = ac_pb_str [] [];;

(************************************************************************)
(* assoc+idem *)

let symb_plus = Symb.symbol_of_string "+"

let assoc_idem_theory = ass symb_plus @ idem symb_plus;;

let assoc_idem = pb assoc_idem_theory;;

(* comp 3 assoc_idem *)

(************************************************************************)
(* semigroup: assoc+neutr *)

let symb_zero = Symb.symbol_of_string "0"

let left_semigroup_theory c e = ass c @ lneu c e;;

let left_semigroup = pb (left_semigroup_theory symb_plus symb_zero);;

(* comp 1 left_semigroup *)

let right_semigroup_theory c e = ass c @ rneu c e;;

let right_semigroup = pb (right_semigroup_theory symb_plus symb_zero);;

(* comp 2 right_semigroup *)

let semigroup_theory c e = ass c @ neu c e;;

let semigroup = pb (semigroup_theory symb_plus symb_zero);;

(* comp 1 semigroup *)

(* commutative semigroup *)

let ac_semigroup = ac_pb [symb_plus] [] (neu symb_plus symb_zero);;

(* comp 2 ac_semigroup *)

(************************************************************************)
(* sets: commutative semigroup + idempotence *)

let set = ac_pb [symb_plus] [] (neu symb_plus symb_zero @ idem symb_plus);;

(* comp 2 set *)

(************************************************************************)
(* group: assoc+neutr+inv *)

let symb_minus = Symb.symbol_of_string "-"

let group_theory c e i = semigroup_theory c e @ inv c i e;;

let group = pb (group_theory symb_plus symb_zero symb_minus);;

(* comp 4 group *)

(* commutative group *)

let ac_group_theory = neu symb_plus symb_zero @ inv symb_plus symb_minus symb_zero;;
let ac_group = ac_pb [symb_plus] [] ac_group_theory;;
(* comp 2 ac_group;; *)

(* with udis *)

let ac_group_theory_udis = udis symb_minus symb_plus symb_plus @ ac_group_theory;;
let ac_group_udis = ac_pb [symb_plus] [] ac_group_theory_udis;;
(* comp 2 ac_group_udis;; *)

(************************************************************************)
(* integers *)

let symb_times = Symb.symbol_of_string "*"

let symb_one = Symb.symbol_of_string "1"

let integer_theory =
  group_theory symb_plus symb_zero symb_minus
  @ semigroup_theory symb_times symb_one
  @ abs symb_times symb_zero
  @ invdis symb_times symb_minus;;

let integer = pb integer_theory;;

(* comp 5 integer *)

(************************************************************************)
(* booleans *)

(*let bool_th = ass "Or" @ inv "Or" "Not" "True" @ neu "Or" "False";;*)

let bool_th = "
  Or (Or x y) z = Or x (Or y z);
  Or (Not x) x = True;
  Or False x = x;
  Or (Not x) (Or x y) = True;
";;

let bool = pb_str bool_th;;

(* comp 5 bool *)

(************************************************************************)
(* insert_add *)

let eqs_and = equations_of_string
  "and True x = x;
   and False x = False;
   and x True = x;
   and x False = False";;

let eqs_eq = equations_of_string
  "eq x x = True;
   eq 0 (Plus x y) = False;
   eq (Plus x y) 0 = False;
   eq (Plus x1 y1) (Plus x2 y2) = and (eq x1 x2) (eq y1 y2)";;

let eqs_if = equations_of_string
  "If True x y = x;
   If False x y = y";;

let eqs_isOpp = equations_of_string
  "isOpp (Opp x) = True;
   isOpp 0 = False;
   isOpp (Plus x y) = False";;

let eqs_getOpp = equations_of_string
  "getOpp (Opp x) = x";;

(************************************************************************)

let eqs_D4 = equations_of_string
  "+ 1 -4 = + -2 -1;
   + 1 -2 = -1;
   - 1 = -1;
   + 1 1 = 2;
   + 2 -4 = -2;
   - 2 = -2;
   + 2 -1 = 1;
   + 2 2 = 4;
   - 4 = -4;
   + 4 -2 = 2;
   + 4 -1 = + 2 1;
   - -1 = 1;
   - -2 = 2;
   - -4 = 4;
   + -1 -1 = -2;
   + -2 -2 = -4";;

let ac_group_eqs = eqnset_of_theory ac_group_theory_udis;;
let ac_group_D4_eqs = EqnSet.union ac_group_eqs eqs_D4;;
let prec_D4 =
  prec_of_list (List.map Symb.symbol_of_string 
  ["0"; "1"; "2"; "4"; "-4"; "-2"; "-1"; "+"; "-"]);;
(* comp 3 (RulSet.empty, ac_group_D4_eqs, RulSet.empty, prec_D4);; *)

(************************************************************************)
(* main *)

Debug.verbose := true;;
Debug.trace "step";;
Debug.trace "rule";;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
