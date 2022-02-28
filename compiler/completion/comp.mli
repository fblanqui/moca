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

(** {4 Knuth-Bendix completion } *)
open Term;;
open Equation;;
open Rule;;
open Useful;;
open Order;;

type ers = EqnSet.t * RulSet.t;;

val fprintf : ers fprintf;;

exception Unorientable of EqnSet.t;;

(** Completion step *)
val step : RulSet.t -> term cmp -> ers -> ers;;

(** Kimited completion. n is the maximal number of completion steps.
    rules are assumed oriented by gt *)
val complete_n : RulSet.t -> term cmp -> int -> ers -> ers * int;;

(** Full completion. may not terminate. rules are assumed oriented by gt *)
val complete : RulSet.t -> term cmp -> ers -> ers * int;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
