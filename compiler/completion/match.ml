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

(* matching *)

open Term;;

type pattern = term;;

open Subs;;

let matching_subs =
  let rec aux s p t =
    match p, t with
      | Var x, t -> check_add x t s
      | App (f, ps), App (g, ts) when f = g -> List.fold_left2 aux s ps ts
      | App _, Var _
      | App _, App _ -> failwith "matching_subs"
  in aux Subs.empty;;

let matches_opt p t =
  try Some (matching_subs p t) with Failure _ -> None;;

let matches p t = Useful.bool_of_opt (matches_opt p t);;

(* insertion ordering: t > u if t contains an instance of u *)

let insert_gt t u = Subterm.exists (matches u) t;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
