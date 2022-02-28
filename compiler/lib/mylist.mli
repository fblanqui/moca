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

(** {4 List functionalities, extending the standard OCaml library } *)

open Useful;;

(** Given a list [l] and a number [i] such that [0 <= i < List.length l],
    returns the triple [l1, x, l2]
    such that [x = List.nth l i] and [l = List.rev l1 @ x :: l2] *)
val split : 'a list -> int -> 'a list * 'a * 'a list;;

(** Replace the i-th element of a list *)
val replace : 'a -> 'a list -> int -> 'a list;;

(** Replace the i-th element x by f(x) *)
val replace_map : ('a -> 'a) -> 'a list -> int -> 'a list;;

(** Position of an element *)
val position : 'a -> 'a list -> int;;

(** Print a list *)
val fprintf_list : 'a fprintf -> ('a list) fprintf;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
