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

(* $Id: configuration.ml,v 1.21 2012-06-04 13:01:22 weis Exp $ *)

(** {3 The current version of Moca.} *)

let moca_version_number = 0.7
and moca_minor_version_number = 0
and moca_sub_version_number = 0
and moca_version_date = "2011-11-01"
;;

let moca_full_version =
  Printf.sprintf "%.1f.%i+%i (%s)"
    moca_version_number moca_minor_version_number
    moca_sub_version_number moca_version_date
;;

let get_verbose, set_verbose =
  let verbose = ref false in
  (fun () -> !verbose),
  (fun () -> verbose := true)
;;

let verbose_printf fmt =
  if get_verbose () then Printf.printf fmt
  else Printf.ifprintf stdout fmt
;;

let verbose_print fmt =
  if get_verbose () then Format.printf fmt
  else Format.ifprintf Format.std_formatter fmt
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
