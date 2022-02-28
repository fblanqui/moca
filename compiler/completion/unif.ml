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

(* unification *)

open Term;;
open Subs;;

let unif =
  let rec aux s = function
    | [] -> s
    | (Var x, Var y) :: l when x = y -> aux s l
    | (Var x, t) :: l | (t, Var x) :: l ->
        if occurs x t then failwith "unif: occur check" else
          let f = apply (subs x t) in
          let feq (u,v) = f u, f v and s = Subs.map f s in
            aux (Subs.add x t s) (List.map feq l)
    | (App (f, ts), App (g, us)) :: l when f = g ->
        aux s (List.fold_left2 (fun r t u -> (t,u) :: r) l ts us)
    | (App _, App _) :: _ -> failwith "unif: distinct head symbols"
  in aux empty
;;

let mgu t u = unif [(t,u)];;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
