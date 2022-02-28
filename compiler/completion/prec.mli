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

(** {4 Precedence definitions } *)

type prec;;

open Symb;;
open Order;;

val compare : prec -> symbol cmp;;

val empty : prec;;

(** Add a new constraint. raise Failure if it is not compatible *)
val add_lt : prec -> symbol -> symbol -> prec;;
val add_eq : prec -> symbol -> symbol -> prec;;
val add_gt : prec -> symbol -> symbol -> prec;;

(** [f1; ..; fn] gives f1 < .. < fn *)
val prec_of_list : symbol list -> prec;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
