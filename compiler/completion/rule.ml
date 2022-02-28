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

(* rewrite rules. the variables of the rhs must be included in the lhs *)

open Term;;

type rule = Mk of term * term;;

open Subs;;
open Rename;;

(* print a rule *)

let fprintf ppf (l,r) = Format.fprintf ppf "%a -> %a" Term.fprintf l Term.fprintf r;;

(* canonical rule representation *)

let mk (l, r) =
  let xs = list_vars_of_terms [l] in
  let rec aux = function
    | Var x -> Var (Mylist.position x xs)
    | App (f, ts) -> App (f, List.map aux ts)
  in Mk (aux l, aux r);;

(* rule comparison (modulo renaming of lhs variables) *)

let compare (Mk (l1,r1)) (Mk (l2,r2)) = Pervasives.compare (l1,r1) (l2,r2);;

let eq e1 e2 = compare e1 e2 = 0;;

(* useful modules *)

let fprintf ppf (Mk (l,r)) = fprintf ppf (l,r);;

module OrdRul = struct
  type t = rule;;
  let compare = compare;;
  let fprintf = fprintf;;
end;;

module RulSet = Myset.Make (OrdRul);;

(* symbols of a set of rules *)

open Symb;;

let symbols_of_rule (Mk (l, r)) = SymbolSet.union (symbols l) (symbols r);;

let symbols_of_rules =
  let f r s = SymbolSet.union (symbols_of_rule r) s in
    fun rs -> RulSet.fold f rs SymbolSet.empty
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
