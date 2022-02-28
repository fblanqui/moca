(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2012,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: file.mli,v 1.13 2012-06-04 13:01:22 weis Exp $ *)

(** {4 File related utilities and configuration elements } *)

(** {1 File management auxiliaries.} *)

type file_name = string
;;
type dir_name = string
;;
type file_extension = string
;;
type module_name = string
;;
(** Various names for specialized versions of [string]. *)

val careful_open_out : file_name -> out_channel
(** An alias for [open_out] that performs correctly on [stdout]. *)
;;

val careful_close_out : out_channel -> unit
(** An alias for [close_out] that performs correctly on [stdout]. *)
;;

exception Unknown_file_extension of string
;;

val change_extension : file_extension -> file_extension -> file_name -> file_name
(** Computes the name of a file when its extension is modified. *)
;;
val change_mlm_extension : file_extension -> file_name -> file_name
;;

val create_lexbuf_from_file : file_name -> Lexing.lexbuf
;;

(** {1 Search path management.} *)

val get_search_path : unit -> dir_name list
;;
val add_dir_to_search_path : dir_name -> unit
;;
val search_mli_file_for_module : module_name -> file_name
;;
val search_ml_file_for_module : module_name -> file_name
;;
val module_of_mli_file : file_name -> module_name
;;

(** {1 Parsing files.} *)

val parse_ml_file : file_name -> Parsetree.structure_item list
;;
val parse_mli_file : file_name -> Parsetree.signature_item list
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
