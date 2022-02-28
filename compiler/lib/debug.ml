(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2012,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

open Useful;;

let get_debug, set_debug =
  let debug = ref false in
  (fun () -> !debug),
  (fun () -> debug := true)
;;

let not_yet_implemented msg =
  failwith (Printf.sprintf "feature not yet implemented: %s" msg)
;;

let tset = ref StrSet.empty;;

let trace s = tset := StrSet.add s !tset;;

let untrace s = tset := StrSet.remove s !tset;;

let is_traced s = get_debug () && StrSet.mem s !tset;;

open Printf;;

let sep = "**************************************************************\n";;

let print fmt =
  if get_debug ()
  then printf fmt
  else ifprintf stdout fmt;;

let output fn fmt =
  if is_traced fn
  then (printf "%s: " fn; printf fmt)
  else ifprintf stdout fmt;;

let output_sep fn fmt =
  if is_traced fn
  then (printf "%s%s: " sep fn; printf fmt)
  else ifprintf stdout fmt;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
