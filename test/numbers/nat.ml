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

(* $Id: nat.ml,v 1.2 2007-11-20 18:58:48 blanqui Exp $ *)

type t = Mk of int;;

let mk x = if x >= 0 then Mk x else failwith "Negative integer";;

let value (Mk x) = x;;

let add (Mk x) (Mk y) = Mk (x+y);;

let prod (Mk x) (Mk y) = Mk (x*y);;
