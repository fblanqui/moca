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

(* $Id: genr_unary.mli,v 1.14 2012-01-31 09:12:58 bonichon Exp $ *)

(** {3 Generation of construction function for unary generators.} *)

val genr_function :
  Otype.constructor_definition -> Genr_base.generated_functions
(** [genr_function g] generates a unary function that
    normalizes terms that are built with [g] as their head constructor.

    We perform here the generation of code for the construction function only in
    the case of a ``true'' unary generator.

    A true unary generator has one argument and this argument is
    not of type [list]. Hence, this argument should not be treated as the
    variable length list of arguments of an associative binary operation.

    The case of an associative binary operator is handled in the module
    [Genr_listary]; we branch to this case when encountering an associative
    operator, since a variable length list of argument cannot exist for a
    true unary operator.
*)
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
