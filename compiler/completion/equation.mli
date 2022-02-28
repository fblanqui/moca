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

(* $Id: equation.mli,v 1.15 2012-01-31 09:12:58 bonichon Exp $ *)

(** {4 Equations for completion } *)

open Term;;
open Useful;;
open Order;;

type eqn = private Mk of term * term;;

val mk : term * term -> eqn;;

(** Equality up to renaming of variables *)
val compare : eqn -> eqn -> int;;

val eq : eqn -> eqn -> bool;;

(** Print an equation into a formatter *)
val fprintf : eqn fprintf;;

(* useful modules *)

module OrdEqn : ORD_PRT with type t = eqn;;

module EqnSet : Myset.S with type elt = eqn;;

val add_non_trivial_pair :
  (term -> term) -> term * term -> EqnSet.t -> EqnSet.t;;

val add_non_trivial : (term -> term) -> eqn -> EqnSet.t -> EqnSet.t;;

val remove_trivial : (term -> term) -> EqnSet.t -> EqnSet.t;;

(** Test if an equation is the commutativity equation *)
val is_commutativity : eqn -> bool;;

val has_commutativity : EqnSet.t -> bool;;

val remove_commutativity : EqnSet.t -> EqnSet.t;;

(** Partition a set of equations into those whose lhs is greater,
    smaller or something else
*)
val partition : term cmp -> EqnSet.t -> EqnSet.t * EqnSet.t * EqnSet.t;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
