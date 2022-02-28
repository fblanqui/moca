(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: eqnrel.mli,v 1.11 2012-06-04 13:36:52 weis Exp $ *)

open Parsetree;;
open Term;;
open Equation;;

(** Convert a set of Moca relations into a set of equations. *)

val eqnset_of_rels : generator -> relations -> EqnSet.t;;

(** Convert a single Moca relation into a set of equations. *)

val eqns_of_rel : generator -> relation -> (term * term) list;;

(** Convert a term into a Code.exp. *)

(*val exp_of_term : term -> exp;;*)

(** Convert a rule into a Code.clause. *)

(*val clause_of_rule : rule -> clause;;*)

(** Completion of a type declaration. *)

type kb_result =
  | Success of (string * type_declaration) list
  | Incomplete
  | Fail of EqnSet.t;;

(* a boolean result indicates a non algebraic rule *)
val completion :
  int -> (string * type_declaration) list -> kb_result * bool;;

val get_kb : unit -> bool;;

val set_kb : unit -> unit;;

val get_kb_limit : unit -> int;;

val set_kb_limit : int -> unit;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
