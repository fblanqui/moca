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

(* $Id: genr_values.mli,v 1.9 2012-04-02 08:37:48 weis Exp $ *)

(** For our semantically meaningful abbrevs for int type. *)
open Tgenr_base;;

(** {Generation of values of a given type definition} *)

val genr_values :
  randomize -> max_length -> nesting_depth ->
  Parsetree.type_declaration -> Parsetree.core_type list -> Code.exp list

(** [genr_values rand length depth td args] where
    [rand]: indicates if random values should be generated or not,
    [length]: the maximum length of the resulting list (maximum number of
              values to generate),
    [depth]: the upper bound for the constructor nesting depth,
    [td]: the type declaration we generate values from,
    [args]: the arguments of the type if polymorphic.

    Generates a list of up to [n] values of the specified type declaration
    and arguments, each one with the specified maximum constructor nesting
    depth (i.e the maximum number of nested constructors). *)
;;

val genr_core_values :
  randomize -> max_length -> nesting_depth ->
  Parsetree.core_type -> Code.exp list

(** [genr_core_values rand length depth td args] where
    [rand]: indicates if random values should be generated or not,
    [length]: the length of the resulting list (number of
              values to generate),
    [depth]: the upper bound for the constructor nesting depth,
    [ty]: the core type type of the generated values. *)
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
