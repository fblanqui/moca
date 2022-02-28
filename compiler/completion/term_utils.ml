(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frederic Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: term_utils.ml,v 1.4 2011-11-03 18:33:40 weis Exp $ *)

open Term
open Symb
;;

let x = Var 0
and y = Var 1
and z = Var 2
and w = Var 3
;;

let symb_of_gen g = symbol_of_string (Otype.qualified_name_of_generator g);;

let app g xs = App (symb_of_gen g, xs);;

let app0 g = app g []
and app1 g x = app g [x]
and app2 g x y = app g [x; y]
;;

let symb_of_lid lid =
  symbol_of_string (Otype.qualified_name_of_longident lid);;

let iapp lid xs = App (symb_of_lid lid, xs);;

let iapp0 lid = iapp lid []
and iapp1 lid x = iapp lid [x]
and iapp2 lid x y = iapp lid [x; y]
;;

let vars =
  let rec aux acc = function
    | n when n > 0 -> aux (Var (pred n) :: acc) (pred n)
    | _ -> acc
  in aux []
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
