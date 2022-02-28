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

(* substitutions *)

open Var;;

module Subs = VarMap;;

open Term;;

(* substitutions are represented as maps from variables to terms
without mapping of the form (x, Var x).

WARNING: this invariant must be preserved ! *)

type subs = term Subs.t;;

let empty = Subs.empty;;
let find = Subs.find;;
let mem = Subs.mem;;

let is_empty s = (s = Subs.empty);;

(* add a new binding only if it is not the identity *)

let add x t s = if t = Var x then s else Subs.add x t s;;

(* map *)

let map f s = Subs.fold (fun x t s' -> add x (f t) s') s Subs.empty;;

(* one-variable substitution *)

let subs x t = add x t Subs.empty;;

(* add to s the mapping (x,t) only if mem s x = false or find s x = t.
raise Failure otherwise *)

let check_add x t s =
  try if t = find x s then s else failwith "check_add"
  with Not_found -> Subs.add x t s;;

(* application of a substitution *)

let apply_var s x = try Subs.find x s with Not_found -> Var x;;

let apply s =
  let rec aux = function
    | Var x as t -> (try Subs.find x s with Not_found -> t)
    | App (f, ts) -> App (f, List.map aux ts)
  in aux;;

(* domain of a substitution *)

let domain s = Subs.fold (fun x _ d -> VarSet.add x d) s VarSet.empty;;

(* restriction of a substitution *)

let restrict s d =
  VarSet.fold (fun x s' -> Subs.add x (apply_var s x) s') d Subs.empty;;

(* restricted composition: domain (comp_res s1 s2) = domain s2 *)

let comp_res s1 s2 =
  Subs.fold (fun x2 t2 s -> add x2 (apply s1 t2) s) s2 Subs.empty;;

(* composition of 2 substitutions:
apply (comp s1 s2) t = apply s1 (apply s2 t) *)

let comp s1 s2 =
  let f1 x1 t1 s = if Subs.mem x1 s2 then s else Subs.add x1 t1 s in
    Subs.fold f1 s1 (comp_res s1 s2);;

(* inverse *)

let inverse =
  let f x t s =
    match t with
      | Var y -> check_add y (Var x) s
      | App _ -> failwith "inverse"
  in fun s -> Subs.fold f s Subs.empty;;

(* print a substitution into a buffer *)

let fprintf_mapping ppf x =
  Format.fprintf ppf "%s: %a\n" (raw_string_of_var x) Term.fprintf;;

let fprintf ppf = Subs.iter (fprintf_mapping ppf);;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
