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

(** {4 Access to debugging elements and resources } *)

val get_debug : unit -> bool;;

val set_debug : unit -> unit;;

(** Raises failure with "not yet implemented" and the string argument. *)
val not_yet_implemented : string -> 'a;;

val trace : string -> unit;;

val untrace : string -> unit;;

val sep : string;;

val print : ('a, out_channel, unit) format -> 'a;;

val output : string -> ('a, out_channel, unit) format -> 'a;;

val output_sep : string -> ('a, out_channel, unit) format -> 'a;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
