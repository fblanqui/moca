(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Laura Lowenthal, projet Protheo, INRIA Lorraine           *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: randomutils.ml,v 1.6 2011-11-04 10:36:13 weis Exp $ *)

(** {Functions for dealing with random values} *)

let ints n bound =
  Listutils.map_from_to (fun _ -> Random.int bound) 1 n succ
;;

let choose_one l =
  let bound = List.length l in
  List.nth l (Random.int bound)
;;

let choose_many n = function
  | [] -> []
  | l -> Listutils.map_from_to (fun _ -> choose_one l) 1 n succ
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
