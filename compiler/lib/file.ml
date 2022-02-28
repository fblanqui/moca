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

(* $Id: file.ml,v 1.13 2012-06-04 13:01:22 weis Exp $ *)

open Printf
;;

type file_name = string
;;
type dir_name = string
;;
type file_extension = string
;;
type module_name = string
;;

let careful_open_out fname =
  if fname = "-" then stdout else open_out fname
;;

let careful_close_out oc =
  if oc != stdout then close_out oc
;;

(** Find the filename with extension [ext2] from a file
    with extension [ext1].
    Being careful with the [stdin]/[stdout] case. *)

exception Unknown_file_extension of string
;;

let change_extension ext1 ext2 fname =
  match fname with
  | "-" -> fname
  | _ ->
    let is_ext1 = Filename.check_suffix fname ext1 in
    if is_ext1 then Filename.chop_extension fname ^ ext2
    else failwith (sprintf "File ``%s'' has no extension ``%s''" fname ext1)
;;

let change_mlm_extension ext2 fname =
  if Filename.check_suffix fname ".mlm"
  then change_extension ".mlm" ext2 fname
  else if Filename.check_suffix fname ".mlms"
  then change_extension ".mlms" ext2 fname
  else raise (Unknown_file_extension fname)
;;

(* Search path for modules (-I option) *)

let get_search_path, add_dir_to_search_path =
  let search_path = ref ["."] in
  (fun () -> !search_path),
  (fun s -> search_path := s :: !search_path)
;;

let create_lexbuf_from_file fname =
  let ic =
    match fname with
    | "-" -> stdin
    | s -> open_in s in
  let ib = Lexing.from_channel ic in
  ib.Lexing.lex_curr_p <-
    { ib.Lexing.lex_curr_p with Lexing.pos_fname = fname };
  ib
;;

let rec search_file fname = function
  | [] -> raise Not_found
  | d :: ds ->
      let abs_fname = Filename.concat d fname in
      if Sys.file_exists abs_fname
      then abs_fname
      else search_file fname ds
;;

let search_mli_file_for_module modname =
  let fname = String.uncapitalize modname ^ ".mli" in
  search_file fname (get_search_path ())
;;

let search_ml_file_for_module modname =
  let fname = String.uncapitalize modname ^ ".ml" in
  search_file fname (get_search_path ())
;;

let module_of_mli_file mli_file =
  let base_name = Filename.basename mli_file in
  String.capitalize (Filename.chop_extension base_name)
;;

let parse_ml_file fn =
  let lb = create_lexbuf_from_file fn in
  Parse.implementation lb
;;

let parse_mli_file fn =
  let lb = create_lexbuf_from_file fn in
  Parse.interface lb
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
