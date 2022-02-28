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

(** {4 Substitutions } *)

open Var;;
open Term;;

type subs;;

(** Identity: substitution with no binding *)
val empty : subs;;

(** Test if a substitution is the identity *)
val is_empty : subs -> bool;;

(** One-variable substitution *)
val subs : var -> term -> subs;;

(** Add a new binding: replace the previous one if any *)
val add : var -> term -> subs -> subs;;

(** Add to s the mapping (x,t) if mem s x = false or find s x = t.
    raise Failure otherwise *)
val check_add : var -> term -> subs -> subs;;

(** Raise Not_found if var is not bound *)
val find : var -> subs -> term;;

(** Test if there is a binding *)
val mem : var -> subs -> bool;;

(** Map a function on bindings *)
val map : (term -> term) -> subs -> subs;;

(** Apply a substitution to a term *)
val apply : subs -> term -> term;;

(** Domain of a substitution *)
val domain : subs -> VarSet.t;;

(** Restriction of a susbtitution to some domain *)
val restrict : subs -> VarSet.t -> subs;;

(** Composition of 2 substitutions:
    apply (comp s1 s2) t = apply s1 (apply s2 t) *)
val comp : subs -> subs -> subs;;

(** Restricted composition: comp_res s1 s2 = restrict (domain s2) (comp s1 s2) *)
val comp_res : subs -> subs -> subs;;

(** Inverse a substitution mapping variables to variables.
    Raise Failure otherwise *)
val inverse : subs -> subs;;

(** Print a substitution into a formatter. *)
val fprintf : Format.formatter -> subs -> unit;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
