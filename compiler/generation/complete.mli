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

(* $Id: complete.mli,v 1.18 2010-09-20 17:48:06 bonichon Exp $ *)

(** {3 Completion of algebraic properties.} *)

val adhoc_completion :
    (string * Parsetree.type_declaration) list
  -> (string * Parsetree.type_declaration) list
;;
(** Some relations on a generator imply relations on other
  generators, that the user cannot or does not need to
  provide. *)
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
