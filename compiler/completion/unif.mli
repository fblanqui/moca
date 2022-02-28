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

(** {4 Unification } *)

open Term;;
open Subs;;

(** Return mgu or raise Failure *)
val unif : (term * term) list -> subs;;

(** Computes the MGU of two terms. *)
val mgu : term -> term -> subs;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
