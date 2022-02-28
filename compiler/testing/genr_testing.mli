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

(* $Id: genr_testing.mli,v 1.10 2011-11-04 10:35:20 weis Exp $ *)

(** {Generation of testing expressions for a moca type declaration.} *)

val genr_test_preamble :
  Tgenr_base.module_name -> Tgenr_base.seed -> Code.structure_item list
;;
(** Generate hte AST preamble for the test file of a module.
    Contains opening of necessary modules and report the test seed number.
*)

val genr_type_test :
  Tgenr_base.seed -> Tgenr_base.num_tests -> Tgenr_base.nesting_depth ->
  (string * Parsetree.type_declaration) list -> Code.structure_item list
;;
(** The global test generation function: it generates the whole
  set of test expressions for a module name and a type definition.
  Usage: [genr_type_test rnd seed ntests val_depth tds] where
    rnd: wether to generate random values or not
    seed: random seed
    ntests: number of tests par equation
    val_depth: maximum value depth, that is, number of nested constructors
    tds: type declarations to test
*)

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
