(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Laura Lowenthal, projet Protheo, INRIA Lorraine           *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: genr_listary_equations.mli,v 1.5 2010-09-20 17:48:07 bonichon Exp $ *)

(** {Generation of testing equations for varyadic constructors.} *)

open Term
open Parsetree
;;

val testing_equations_of_rels : generator -> relations -> (term * term) list
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
