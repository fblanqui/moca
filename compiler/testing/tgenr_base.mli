(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Laura Lowenthal, projet Protheo, INRIA Lorraine           *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: tgenr_base.mli,v 1.15 2012-04-02 08:37:48 weis Exp $ *)

type module_name = string
and randomize = bool
and max_length = int
and nesting_depth = int
and seed = int
and num_tests = int;;

(** {Basic functions for value generation.} *)

val is_known_and_listary : Parsetree.generator -> 'a list -> bool

(** Decides if a generator (with the arguments it should be applied to) is
    known and listary.

    [is_known_and_listary g args] returns true when

    - [g] is defined (by the user) to have a single list as argument,

    - [args] has more than one element. This is for the purpose of test
    generation only, where generators must be treated as listary only if
    they are applied to many arguments. *)
;;

val is_known_and_listary_name : string -> 'a list -> bool
(** Same as above, indicating the generator name instead of the generator *)
;;

val mk_equality : Code.exp -> Code.exp -> Code.exp
(** Creates an expression representing the application of the equality operator *)
;;

val mk_int : int -> Code.exp
(** Creates a value representing an integer. *)
;;

val mk_char : char -> Code.exp
(** Creates a value representing a character. *)
;;

val mk_string : string -> Code.exp
(** Creates a value representing a string. *)
;;

val mk_float : float -> Code.exp
(** Creates a value representing a float. *)
;;

val mk_int32 : int32 -> Code.exp
(** Creates a value representing an int32. *)
;;

val mk_int64 : int64 -> Code.exp
(** Creates a value representing an int 64. *)
;;

val mk_nativeint : nativeint -> Code.exp
(** Creates a value representing an native integer. *)
;;

val mk_tuple : Code.exp list -> Code.exp
(** Creates a value representing a tuple with the specified expressions. *)
;;

val mk_list : Code.exp list -> Code.exp
(** Creates a value representing a list with the specified expressions. *)
;;

val mk_generator : string -> Location.t -> Parsetree.generator
(** Creates a generator with the given name and location *)
;;

val mk_construction : string -> Code.exp list -> Code.exp
(** Creates an expression in [Code.exp] representing a generator with the
    specified name and arguments. *)
;;

val mk_non_listary_construction : string -> Code.exp list -> Code.exp
(** Creates an expression in Code.exp representing a generator with the
    specified name and arguments.

    Treats the generator as non listary, even if it has a single list argument. *)
;;

val genr_ints : randomize -> max_length -> Code.exp list
(** Creates the [max_length] specified number of distinct integer expressions.
    If the boolean argument is true, then exressions must be generated randomly,
    otherwise they are generated sequentially. *)
;;

val genr_chars : randomize -> max_length -> Code.exp list
(** Same as above for characters. *)
;;

val genr_strings : randomize -> max_length -> Code.exp list
(** Same as above for strings. *)
;;

val genr_floats : randomize -> max_length -> Code.exp list
(** Same as above for floats. *)
;;

val genr_int32s : randomize -> max_length -> Code.exp list
(** Same as above for [int32]. *)
;;

val genr_int64s : randomize -> max_length -> Code.exp list
(** Same as above for [int64]. *)
;;

val genr_nativeints : randomize -> max_length -> Code.exp list
(** Same as above for [nativeint]. *)
;;

val genr_units : randomize -> max_length -> Code.exp list
(** Creates at most the specified number of distinct unit expressions, i.e.,
    zero or one. *)
;;

(*
  Local Variables:
  compile-command: "cd ..; make"
  End:
*)
