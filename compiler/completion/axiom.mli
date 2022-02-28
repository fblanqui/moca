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

(** {4 Representations of Moca equations } *)
open Symb;;

(* builtin equations in moca *)

type side = Left | Right;;

type axiom =
  | Com of symbol
  | Ass of symbol
  | Nil of symbol * symbol
  | Idem of symbol
  | Neu of side * symbol * symbol
  | Abs of side * symbol * symbol
  | Inv of side * symbol * symbol * symbol
  | Dis of side * symbol * symbol
  | InvDis of side * symbol * symbol
  | UDis of symbol * symbol * symbol
  | Invol of symbol
  | UNil of symbol * symbol
  | UIdem of symbol
  | InvNeu of symbol * symbol * symbol
;;

type theory = axiom list;;

val com : symbol -> theory;;
val ass : symbol -> theory;;
val nil : symbol -> symbol -> theory;;
val idem : symbol -> theory;;
val lneu : symbol -> symbol -> theory;;
val rneu : symbol -> symbol -> theory;;
val neu : symbol -> symbol -> theory;;
val labs : symbol -> symbol -> theory;;
val rabs : symbol -> symbol -> theory;;
val abs : symbol -> symbol -> theory;;
val linv : symbol -> symbol -> symbol -> theory;;
val rinv : symbol -> symbol -> symbol -> theory;;
val inv : symbol -> symbol -> symbol -> theory;;
val ldis : symbol -> symbol -> theory;;
val rdis : symbol -> symbol -> theory;;
val dis : symbol -> symbol -> theory;;
val linvdis : symbol -> symbol -> theory;;
val rinvdis : symbol -> symbol -> theory;;
val invdis : symbol -> symbol -> theory;;
val udis : symbol -> symbol -> symbol -> theory;;
val invol : symbol -> theory;;
val unil : symbol -> symbol -> theory;;
val uidem : symbol -> theory;;
val invneu : symbol -> symbol -> symbol -> theory;;

open Equation;;

val eqnset_of_theory : theory -> EqnSet.t;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
