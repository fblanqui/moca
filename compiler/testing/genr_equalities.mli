(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Laura Lowenthal, projet Protheo, INRIA Lorraine           *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: genr_equalities.mli,v 1.11 2012-01-30 18:22:45 weis Exp $ *)

(** {Generation of equalities on values} *)

(** From the relations of a generator of a given relational type definition,
   we deduce equalities that must hold if the construction functions are
   correct. We then generate the code that tests those equalities for a set
   of randomly chosen values.
*)

val genr_values_and_equalities :
  int -> (Parsetree.core_type -> int -> Code.exp list) ->
  string * Parsetree.core_type list * Parsetree.relations * Location.t ->
  Code.exp list
(** Given
  a number [n] of equalities to generate,
  a function generating values of a given type, and
  a constructor definition,
  [genr_values_and_equalities] generates the code to test the validity of
  the equalities that must hold, according to the relations specified for the
  constructor.

  For each equation deduced from the relations [n] equalities
  are generated.

  Equality testing expressions have the following form:

    let v1 = example_value1 in
    let v2 = example_value2 in

    ... in

    left_side = right_side
*)
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
 End:
*)
