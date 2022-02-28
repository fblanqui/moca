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

(* $Id: Exp *)

(** {4 Useful functions, modules and types } *)

val bool_of_opt : 'a option -> bool;;

module StrMap : Map.S with type key = string;;
module StrSet : Set.S with type elt = string;;

module IntMap : Map.S with type key = int;;
module IntSet : Set.S with type elt = int;;

type 'a fprintf = Format.formatter -> 'a -> unit;;

module type ORD_PRT = sig
  type t;;
  val compare : t -> t -> int;;
  val fprintf : t fprintf;;
end;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
