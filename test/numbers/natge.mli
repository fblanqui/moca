(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: natge.mli,v 1.3 2007-11-21 12:49:59 blanqui Exp $ *)

module type S =
sig
  type t = private Mk of int
  val mk : int -> t
  val value : t -> int
  val add : t -> t -> t
  val prod : t -> t -> t
end;;

module Make : functor (M : sig val lower_bound : Nat.t end) -> S;;

(* integers bigger or equal to 1 *)
module Natge1 : S;;

(* integers bigger or equal to 2 *)
module Natge2 : S;;
