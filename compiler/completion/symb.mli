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

(** {4 Symbols in completion } *)

type symbol;;

module SymbolSet : Set.S with type elt = symbol;;
module SymbolMap : Map.S with type key = symbol;;

(** Check if a string is represented by a known symbol,
    associate it with a new symbol otherwise *)
val symbol_of_string : string -> symbol;;

(** Return the string represented by some symbol *)
val string_of_symbol : symbol -> string;;

(** Clear the table of known symbols *)
val clear_symbols : unit -> unit;;

val pr_symbol : Format.formatter -> symbol -> unit
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
