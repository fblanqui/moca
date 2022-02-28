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

(** {4 Normalization of completion terms } *)
open Term;;
open Rule;;

(** Compute Some reduct or None *)

val red_opt : RulSet.t -> term -> term option;;

(** Normalization function *)

val norm : RulSet.t -> term -> term;;

(** Test reducibility *)
val is_red : RulSet.t -> term -> bool;;
val is_norm : RulSet.t -> term -> bool;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
