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

(* $Id: natge.ml,v 1.3 2007-11-21 12:49:59 blanqui Exp $ *)

module type S =
sig
  type t = private Mk of int
  val mk : int -> t
  val value : t -> int
  val add : t -> t -> t
  val prod : t -> t -> t
end;;

module Make (M : sig val lower_bound : Nat.t end) =
struct

  type t = Mk of int

  let lb = Nat.value M.lower_bound

  let mk x =
    if x >= lb then Mk x
    else failwith ("Integer smaller than " ^ string_of_int lb)

  let value (Mk x) = x
  let add (Mk x) (Mk y) = Mk (x+y)
  let prod (Mk x) (Mk y) = Mk (x*y)

end;;

module Natge1 = Make (struct let lower_bound = Nat.mk 1 end);;

module Natge2 = Make (struct let lower_bound = Nat.mk 2 end);;
