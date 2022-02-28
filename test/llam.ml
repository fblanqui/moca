(***********************************************************************)
(*                                                                     *)
(*                           Moca                                      *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id: llam.ml,v 1.1 2007-04-24 09:46:36 blanqui Exp $ *)

(* The type of lambda terms and
   the abstraction algorithm to a combinator tree. *)

open Lam;;

let rec mem_free_var x = function
  | Llam (y, b) -> x <> y && mem_free_var x b
  | Lvar y when x = y -> true
  | Lvar y -> false
  | Lapp (f, arg) -> mem_free_var x f || mem_free_var x arg;;

let rec abs_lam = function
  | Lvar s -> Skiv.var s
  | Lapp (f, arg) -> Skiv.app (abs_lam f, abs_lam arg)
  | Llam (x, Lvar y) when x = y -> Skiv.i
  | Llam (x, b) ->
      let c = abs_lam b in
      if mem_free_var x b
      then Skiv.app (Skiv.s, c)
      else Skiv.app (Skiv.k, c);;
