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

open Rule;;
open Match;;
open Subs;;
open Useful;;
open Subterm;;

(* test whether a term matches a lhs.
return Some rhs and substitution or None *)

let hd_one_red_opt (Mk (l,r)) t =
  try let s = matching_subs l t in Some (apply s r)
  with Failure _ -> None;;

(* test whether a term matches a lhs *)

let hd_red_opt rs t = RulSet.find_opt (fun e -> hd_one_red_opt e t) rs;;

(* compute Some reduct or None *)

let red_opt rs t =
  match Subterm.find_opti (fun u _ -> hd_red_opt rs u) t with
    | Some (u, p) -> Some (replace t p u)
    | _ -> None;;

(* test whether a term is reducible *)

let is_red rs t = bool_of_opt (red_opt rs t);;

let is_norm rs t = not (is_red rs t);;

(* compute normal form *)

let norm rs =
  let rec aux t =
    match red_opt rs t with
      | Some u -> aux u
      | None -> t
  in aux;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
