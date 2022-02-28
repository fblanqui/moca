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

(* $Id: cantor_op.mli,v 1.1 2007-11-22 15:35:28 weis Exp $ *)

(** Cantor's ordinals: w^x1.k1 + ... + w^xn.kn with x1 > ... > xn *)

val bprintf : Buffer.t -> Cantor.t -> unit
;;
val add : Cantor.t * Cantor.t -> Cantor.t
;;
val dot : Peano.t -> Cantor.t -> Cantor.t -> Cantor.t
;;
val mul : Cantor.t * Cantor.t -> Cantor.t
;;
