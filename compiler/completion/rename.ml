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

type renaming = int;;

(* apply a renaming to a term *)

let rename z =
  if z = 0 then fun t -> t
  else
    let rec aux = function
      | Var x -> Var (x+z)
      | App (f, ts) -> App (f, List.map aux ts)
    in aux;;

(* variable range of a term *)

let min_max (i,s) (u,v) = min i u, max s v;;

let rec range = function
  | Var x -> x, x
  | App (_, ts) -> ranges (0,0) ts

and ranges acc = function
  | [] -> acc
  | t :: ts -> ranges (min_max acc (range t)) ts;;

(* renaming away from a term *)

let delta t = let inf, sup = range t in sup - inf;;

let renaming_away t = delta t + 1;;

(* extend a renaming *)

let extend_away r t = max r (renaming_away t);;

(* renaming away from a list of terms *)

let renaming_aways =
  let rec aux acc = function
    | [] -> acc
    | t :: ts -> aux (extend_away acc t) ts
  in aux 0;;

(* equality up to variable renaming.
if eq_opt t1 t2 = Some s then apply s t1 = t2 *)

open Match;;

let eq_opt t1 t2 =
  try
    let s1 = matching_subs t1 t2 and _ = matching_subs t2 t1 in Some s1
  with Failure _ -> None;;

let eq t1 t2 =
  try
    let _ = matching_subs t1 t2 and _ = matching_subs t2 t1 in true
  with Failure _ -> false;;

let compare t1 t2 =
  try
    let _ = matching_subs t1 t2 and _ = matching_subs t2 t1 in 0
  with Failure _ -> Pervasives.compare t1 t2;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
