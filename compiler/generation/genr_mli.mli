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

(* $Id: genr_mli.mli,v 1.8 2012-01-31 09:12:58 bonichon Exp $ *)

(** {4 Generate the interface file of the Moca algebraic datatype } *)

val genr_private_type_declarations :
  bool -> Format.formatter -> (string * Parsetree.type_declaration) list -> unit
;;

val genr_public_type_declarations :
  bool -> Format.formatter -> (string * Parsetree.type_declaration) list -> unit
;;

val genr_mli :
  bool -> Format.formatter -> (string * Parsetree.type_declaration) list -> unit
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
