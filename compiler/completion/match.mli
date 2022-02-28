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

(** {4 Matching for completion } *)

open Term;;

type pattern = term;;

open Subs;;

(** Return matching substitution or raise Failure *)
val matching_subs : pattern -> term -> subs;;

val matches : pattern -> term -> bool;;
val matches_opt : pattern -> term -> subs option;;

(** Insertion ordering: t > u if t contains an instance of u *)
val insert_gt : term -> term -> bool;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
