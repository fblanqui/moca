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

(* $Id: comp.ml,v 1.24 2012-04-02 09:27:25 weis Exp $ *)

open Cp;;
open Equation;;
open Rule;;
open Subs;;
open Match;;
open Order;;
open Configuration;;
open Norm;;

type ers = EqnSet.t * RulSet.t;;

let norm = Norm_ml.norm;;

(* combinators for applying completion rules *)

let fprintf ppf (es,rs) =
  Format.fprintf ppf "equations:\n%a\nrules:\n%a\nequations: %i, rules: %i\n%!"
    EqnSet.fprintf es RulSet.fprintf rs
    (EqnSet.cardinal es) (RulSet.cardinal rs)
;;

let apply s rule x =
  verbose_printf "%s begin\n" s;
  let y = rule x in
  verbose_print "%s end\n%a\n" s fprintf y;
  y
;;

let repeat rule =
  let rec aux ers =
    match rule ers with
    | Some ers -> aux ers
    | _ -> ers in
  aux
;;

let repeat s rule ers =
  verbose_printf "%s begin\n" s;
  let ers = repeat rule ers in
  verbose_print "%s end\n%a" s fprintf ers;
  ers
;;

(* rule delete. delete all trivial equations l=l at once *)

(*let delete =
  let f (l,r) = l <> r in
    fun (es,rs) -> EqnSet.filter f es, rs
;;

let delete = apply "delete" delete;;*)

(* rule simplifity. simplify all equations at once *)

(*let simplify (es,rs) =
  let f (l,r) = (norm rs l, norm rs r) in
    EqnSet.map f es, rs
;;

let simplify = apply "simplify" simplify;;*)

(* combination of simplify and delete *)

(*let del_simp ers = delete (simplify ers);;*)

let del_simp (es,rs) = remove_trivial (norm rs) es, rs;;

let del_simp = apply "del_simp" del_simp;;

(* rule orient. orient all possible equations at once.
raise Not_found if there is no orientable equation *)

exception Unorientable of EqnSet.t;;

let add (Equation.Mk (l,r)) = RulSet.add (Rule.mk (l,r));;
let add_rev (Equation.Mk (l,r)) = RulSet.add (Rule.mk (r,l));;

let orient cmp (es,rs) =
  let esgt, eslt, es2 = Equation.partition cmp es in
    if EqnSet.is_empty esgt && EqnSet.is_empty eslt
    then raise (Unorientable es)
    else (es2, EqnSet.fold add esgt (EqnSet.fold add_rev eslt rs))
;;

let orient cmp = apply "orient" (orient cmp);;

(* rule compose: try to reduce a rhs *)

let compose (es,rs) =
  let f (Mk (l,r)) = mk (l, norm rs r) in
  es, RulSet.map f rs
;;

let compose = apply "compose" compose;;

(* test whether a rule is strictly greater than another rule *)

let rule_gt cmp (Mk (l1,r1)) (Mk (l2,r2)) =
  match Rename.eq_opt l1 l2 with
  | Some s -> cmp (Subs.apply s r1) r2 = Greater
  | _ -> insert_gt l1 l2
;;

(* rule collapse: try to reduce a lhs *)

let collapse_one cmp (es,rs) =
  let p ((Mk (l,r)) as lr) =
    let rs2 = RulSet.filter (rule_gt cmp lr) rs in
    match red_opt rs2 l with
    | Some l2 ->
      Some (EqnSet.add (Equation.mk (l2,r)) es, RulSet.remove lr rs)
    | _ -> None in
  RulSet.find_opt p rs
;;

let collapse cmp = repeat "collapse" (collapse_one cmp);;

(*let collapse_one cmp = apply_opt_once "collapse" (collapse_one cmp);;*)

(* rule deduce *)

let deduce acs (es,rs) = add_cps (norm rs) es (RulSet.union acs rs), rs;;

let deduce acs = apply "deduce" (deduce acs);;

(* completion procedure *)

let step acs cmp ers =
  deduce acs (del_simp (collapse cmp (compose (orient cmp ers))))
;;

let complete_n acs cmp n =
  let rec aux i ((es,_) as ers) =
    if EqnSet.is_empty es || i = n then ers, i else
    begin
      verbose_printf "%sstep %d begin\n" Debug.sep (i + 1);
      let ers = step acs cmp ers in
      verbose_print "step %d end\n%a" (i + 1) fprintf ers;
      aux (i + 1) ers
    end in
  aux 0
;;

let complete acs cmp = complete_n acs cmp 0;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
