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

(* $Id: genr_pattern.mli,v 1.9 2010-09-20 17:48:06 bonichon Exp $ *)

(** {3 Pattern generation for Caml code clauses.} *)

val remove_topconstr : Parsetree.pattern -> Parsetree.pattern;;

val add_underscores : Parsetree.pattern -> Parsetree.pattern;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
