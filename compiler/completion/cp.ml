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

open Term;;
open Rename;;
open Subs;;
open Subterm;;
open Unif;;
open Rule;;
open Equation;;

let cp norm (Rule.Mk (l1,r1)) (Rule.Mk (l2,r2)) acc =

  (* rename (l2,r2) with variables away from (l1,r1) *)
  let rho = renaming_aways [l1; r1] in
  let l2 = rename rho l2 and r2 = rename rho r2 in

  (* non-variable subterms of l1 that unifies with l2 *)
  let f t p acc =
    match t with
      | Var _ -> acc
      | App _ as t ->
	  (try
	     let s = mgu l2 t in
	       add_non_trivial_pair norm
		 (apply s r1, replace (apply s l1) p (apply s r2)) acc
	   with Failure _ -> acc) in
  let acc = Subterm.foldi f l1 acc in

  (* non-variable and strict subterms of l2 that unifies with l1 *)
  let f t p acc =
    match t, p with
      | Var _, _ | _, [] -> acc
      | App _ as t, _ ->
	  (try
	     let s = mgu l1 t in
	       add_non_trivial_pair norm
		 (apply s r2, replace (apply s l2) p (apply s r1)) acc
	   with Failure _ -> acc) in
  Subterm.foldi f l2 acc;;

(* critical pairs of a set of rules *)

let add_cps_with norm rs r1 acc = RulSet.fold (cp norm r1) rs acc;;

let add_cps norm acc rs = RulSet.fold (add_cps_with norm rs) rs acc;;

let cps norm = add_cps norm EqnSet.empty;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
