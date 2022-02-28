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

(* $Id: genr_sharing.mli,v 1.23 2012-01-31 09:12:58 bonichon Exp $ *)

(** {4 Generation of functions to get maximal sharing for the values of a given
    type }
*)

val print_sharing_mli_preamble : Format.formatter -> unit
;;

val print_sharing_ml_preamble : Format.formatter -> unit
;;

val print_sharing : Format.formatter ->
  (string * Parsetree.type_declaration) list -> unit
;;

val add_info_arg_to_type_declaration :
  Parsetree.type_declaration -> Parsetree.type_declaration
;;

val print_type_private_structure_items : Format.formatter ->
  (string * Parsetree.type_declaration) list -> unit
(** Prints the structure items defined in the private type definition. *)
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
