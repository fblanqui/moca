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

(** {4 Basic functions on terms } *)

open Term;;

(** Fold among subterms *)
val fold : (term -> 'a -> 'a) -> term -> 'a -> 'a;;

(** Find a subterm satisfying some predicate. raise Not_found otherwise *)
val find : (term -> bool) -> term -> term;;

(** Find Some subterm satisfying some predicate or None *)
val find_opt : (term -> 'a option) -> term -> 'a option;;

(** Test whether there exits a subterm satisfying some predicate *)
val exists : (term -> bool) -> term -> bool;;

(** Positions in a term *)
type position = int list;;

(** Subterm at position p *)
val subterm_pos : term -> position -> term;;

(** [replace t p u] replaces the subterm of t at position p by u *)
val replace : term -> position -> term -> term;;

(** Functions taking into account the position *)
val foldi : (term -> position -> 'a -> 'a) -> term -> 'a -> 'a;;
val findi : (term -> position -> bool) -> term -> term * position;;
val find_opti : (term -> position -> 'a option) -> term -> ('a * position) option;;
val existsi : (term -> position -> bool) -> term -> bool;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
