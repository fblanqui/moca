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

(** {4 Variable renaming } *)

open Term;;

type renaming;;

(** Apply a renaming to a term *)
val rename : renaming -> term -> term;;

(** Renaming away from a term *)
val renaming_away : term -> renaming;;
val renaming_aways : term list -> renaming;;

(** Extend a renaming *)
val extend_away : renaming -> term -> renaming;;


open Subs;;

(** Equality up to variable renaming.
    if eq_opt t1 t2 = Some s then apply s t1 = t2
*)
val eq_opt : term -> term -> subs option;;
val eq : term -> term -> bool;;

val compare : term -> term -> int;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
