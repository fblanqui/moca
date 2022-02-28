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

(* $Id: nat.mli,v 1.2 2007-11-20 18:58:48 blanqui Exp $ *)

type t = private Mk of int;;

val mk : int -> t;;

val value : t -> int;;

val add : t -> t -> t;;

val prod : t -> t -> t;;
