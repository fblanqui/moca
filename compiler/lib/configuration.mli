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

(* $Id: configuration.mli,v 1.17 2012-06-04 13:01:22 weis Exp $ *)

(** {4 Configuration elements for Moca } *)

(** {1 The current version of Moca.} *)

val moca_version_number : float
;;
val moca_minor_version_number : int
;;
val moca_full_version : string
;;

(** {1 Verbose mode } *)

val get_verbose : unit -> bool
;;
val set_verbose : unit -> unit
;;
val verbose_printf : ('a, out_channel, unit) format -> 'a
;;
val verbose_print : ('a, Format.formatter, unit) format -> 'a
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
