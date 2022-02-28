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

(* variables *)

type var = int;;

open Useful;;

module VarSet = IntSet;;
module VarMap = IntMap;;

let raw_string_of_var n =
  assert (n >= 0);
  if n >= 0 then "x" ^ string_of_int n else "y" ^ string_of_int (-n);;

let string_of_var v_int =
  assert (v_int >= 0);
  match v_int with
  | 0 -> "x"
  | 1 -> "y"
  | 2 -> "z"
  | 3 -> "t"
  | n when n >= 0 -> "x" ^ string_of_int (n - 3)
  | n -> "y" ^ string_of_int (-n)
;;

let fresh_string stmap n =
  let s0 = string_of_var n in
  let str k = s0 ^ "_" ^ string_of_int k in
  let rec aux k = if StrMap.mem (str k) stmap then aux (k + 1) else k in
  if StrMap.mem s0 stmap then str (aux 0) else s0
;;

let var_of_string, string_of_var, clear_vars, fresh_var =
  let vamap = ref VarMap.empty
  and stmap = ref StrMap.empty
  and index = ref 0 in
  let update s n =
    stmap := StrMap.add s n !stmap;
    vamap := VarMap.add n s !vamap
  in
  (fun s ->
    (try StrMap.find s !stmap
     with Not_found -> incr index; update s !index; !index)),
  (fun n ->
    (try VarMap.find n !vamap
     with Not_found -> let s = fresh_string !stmap n in update s n; s)),
  (fun () ->
    vamap := VarMap.empty; stmap := StrMap.empty; index := 0),
  (fun () ->
    incr index; let s = fresh_string !stmap !index in update s !index;  !index)
;;

(* old code:

let var_of_string, clear_vars, fresh_var =
  let stmap = ref StrMap.empty
  and n = ref 0 in
    (fun s ->
       (try StrMap.find s !stmap
	with Not_found ->
	  incr n;
	  stmap := StrMap.add s !n !stmap;
	  !n)),
    (fun () ->
       stmap := StrMap.empty;
       n := 0),
    (fun () ->
       incr n;
       !n)
;;
*)

let pr_var ppf v = Format.fprintf ppf "%s(%i)" (string_of_var v) v;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)

