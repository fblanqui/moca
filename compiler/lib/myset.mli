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

(** {4 Useful functions on sets, extending the OCaml library Set functor } *)

open Useful;;

module type S = sig

  include Set.S;;

  (** map on sets *)
  val map : (elt -> elt) -> t -> t;;

  (** Set of elements y such that f x = Some y for some x in xs.
      raise Not_found if f x = None for all x in xs *)
  val map_filter : (elt -> elt option) -> t -> t;;

  (** Provide an element satisfying p. raise Not_found otherwise *)
  val find : (elt -> bool) -> t -> elt;;

  (** Provide Some element satisfying p or None *)
  val find_opt : (elt -> 'a option) -> t -> 'a option;;

  (** Build a set from a list of elements *)
  val of_list : elt list -> t;;
  val of_list_map : ('a -> elt) -> 'a list -> t;;

  (** Print a set into a buffer *)
  val fprintf : t fprintf;;

  (** Union of the sets obtained by mapping f on some list *)
  val union_map : ('a -> t) -> 'a list -> t;;

end;;

module Make : functor (X : ORD_PRT) -> S with type elt = X.t;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
