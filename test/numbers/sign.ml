(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: sign.ml,v 1.1 2007-11-21 12:49:59 blanqui Exp $ *)

type t = Neg | Pos;;

let prod s1 s2 = if s1 = s2 then Pos else Neg;;

let bprintf ob = function
  | Pos -> ()
  | Neg -> Printf.bprintf ob "-";;
