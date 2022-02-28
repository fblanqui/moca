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

(* $Id: gentest.mli,v 1.7 2012-06-04 13:01:22 weis Exp $ *)

(** {3 To automatically test functions.} *)

(** {6 Tools for regular tests.} *)

(** These are functions provided to test functions that do not fail. *)

val testing : string -> unit;;
(** [testing modname] is supposed to be the first command at the
    beginning of a module tests sequence. It prints a message and
    resets the local test counter. *)

val test : bool -> unit;;
(** Tests if the expression argument is true.
    Otherwise a message is printed, and a global failure is
    recorded, which is reported at the program exit. *)

val testl : int -> bool -> unit;;
(** Same as above, given an integer label which should be greater than the
    current test number (starting with 0). *)

val testi : int -> bool -> unit;;
(** Same as above, given an integer index which should be the current
    test number (starting with 0). *)

(** {6 Tools for tests that raise an exception.} *)

(** Tests with exception [Failure]. *)

val test_raises_this_failure : string -> ('a -> 'b) -> 'a -> bool;;
(** [test_raises_this_failure s f x] Applies [f] to [x] and checks
    that the evaluation indeed raises exception [Failure s]. *)

val test_raises_some_failure : ('a -> 'b) -> 'a -> bool;;
(** [test_raises_some_failure f x] Applies [f] to [x] and checks
    that the evaluation indeed raises the exception [Failure]. *)

(** Tests with general exceptions. *)

val test_raises_this_exc : exn -> ('a -> 'b) -> 'a -> bool;;
(** [test_raises_this_exc exc f x] Applies [f] to [x] and checks that
    the evaluation indeed raises exception [exc]. *)

val test_raises_some_exc : ('a -> 'b) -> 'a -> bool;;
(** [test_raises_some_exc f x] Applies [f] to [x] and checks that
    the evaluation indeed raises some exception. *)

val test_raises_exc_p : (exn -> bool) -> ('a -> 'b) -> 'a -> bool;;
(** [test_raises_exc_p p f x] Applies f to x and checks that the
    evaluation indeed raises an exception that verifies the predicate
    [p]. *)
