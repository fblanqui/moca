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

(** {4 Critical pairs } *)
open Rule;;
open Equation;;
open Term;;

(** Critical pairs of a set of rules *)
val add_cps : (term -> term) -> EqnSet.t -> RulSet.t -> EqnSet.t;;

val cps : (term -> term) -> RulSet.t -> EqnSet.t;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
