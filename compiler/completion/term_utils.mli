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

(* $Id: term_utils.mli,v 1.4 2012-01-31 09:12:58 bonichon Exp $ *)

(** {4 Common functions for building terms} *)

open Term;;

val x : term;;
val y : term;;
val z : term;;
val w : term;;

val app : Parsetree.generator -> term list -> term

val app0 : Parsetree.generator -> term
val app1 : Parsetree.generator -> term -> term
val app2 : Parsetree.generator -> term -> term -> term
;;

val iapp : Longident.t -> term list -> term

val iapp0 : Longident.t -> term
val iapp1 : Longident.t -> term -> term
val iapp2 : Longident.t -> term -> term -> term
;;

val vars : int -> term list
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
