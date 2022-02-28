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

(* $Id: equation.ml,v 1.15 2012-01-30 18:19:38 weis Exp $ *)

(* Equations *)

open Term;;
open Order;;

type eqn = Mk of term * term;;

(* canonical representation of an equation *)

let mk (l, r) =
  let xs = list_vars_of_terms [l; r] in
  let rec aux = function
    | Var x -> Var (Mylist.position x xs)
    | App (f, ts) -> App (f, List.map aux ts)
  in Mk (aux l, aux r);;

let compare (Mk (l1,r1)) (Mk (l2,r2)) = Pervasives.compare (l1,r1) (l2,r2);;

let eq e1 e2 = compare e1 e2 = 0;;

(* print an equation *)

let fprintf ppf (Mk (l,r)) =
  Format.fprintf ppf "%a = %a" Term.fprintf l Term.fprintf r;;

(* useful modules *)

module OrdEqn = struct
  type t = eqn;;
  let compare = compare;;
  let fprintf = fprintf;;
end;;

module EqnSet = Myset.Make (OrdEqn);;

let add_non_trivial_pair f (l,r) es =
  let l = f l and r = f r in
    if l = r then es else EqnSet.add (mk (l,r)) es;;

let add_non_trivial f (Mk (l,r)) = add_non_trivial_pair f (l,r);;

let remove_trivial f es = EqnSet.fold (add_non_trivial f) es EqnSet.empty;;

(* test if an equation is commutativity *)

let is_commutativity = function
  | App (f1, [Var x1; Var y1]), App (f2, [Var y2; Var x2]) ->
      f1 = f2 && x1 = x2 && y1 = y2
  | _ -> false;;

let is_commutativity (Mk (l,r)) = is_commutativity (l,r);;

let has_commutativity = EqnSet.exists is_commutativity;;

let remove_commutativity = EqnSet.filter (fun e -> not (is_commutativity e));;

(* partition a set of equations into those whose lhs is greater,
smaller or something else *)

let partition cmp =
  let rec aux (Mk (l,r) as x) (g,s,o) =
    match cmp l r with
      | Greater -> EqnSet.add x g, s, o
      | Smaller -> g, EqnSet.add x s, o
      | Equal | Uncomparable -> g, s, EqnSet.add x o
  in fun es -> EqnSet.fold aux es (EqnSet.empty, EqnSet.empty, EqnSet.empty);;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
