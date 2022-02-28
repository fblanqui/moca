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

(* $Id: tgenr_base.ml,v 1.21 2012-04-02 09:27:26 weis Exp $ *)

type module_name = string
and randomize = bool
and max_length = int
and nesting_depth = int
and seed = int
and num_tests = int;;

(** {Basic functions for value generation} *)

open Code
open Parsetree
;;

(** Creates an expression in Code.exp representing a tuple *)
let mk_tuple exps = Tuple exps

(** Creates an expression in Code.exp representing a list *)
and mk_list exps = List exps

(** Creates a generator with the given name and location *)
and mk_generator name loc = {
  pgen_desc = Longident.Lident name;
  pgen_loc = loc;
}
;;

(** Creates an expression in Code.exp representing a constant *)
let mk_constant c =
  Ocaml_expression {
    pexp_desc = Pexp_constant c;
    pexp_loc = Location.none;
  }
;;

let mk_int n = mk_constant (Asttypes.Const_int n)
and mk_char ch = mk_constant (Asttypes.Const_char ch)
and mk_string str = mk_constant (Asttypes.Const_string str)
and mk_float x = mk_constant (Asttypes.Const_float (string_of_float x))
and mk_int32 n = mk_constant (Asttypes.Const_int32 n)
and mk_int64 n = mk_constant (Asttypes.Const_int64 n)
and mk_nativeint n = mk_constant (Asttypes.Const_nativeint n)
and mk_unit () = mk_tuple []
;;

let mk_non_listary_construction name args =
  let gen_function_name = Genr_base.construction_function_name name in
  let gen_function_ident = make_Constant gen_function_name in
  match args with
  | [] -> gen_function_ident
  | [_] -> Apply (gen_function_ident, args)
  | _ -> Apply (gen_function_ident, [Tuple args])
;;

let mk_listary_construction name args =
  let gen_function_name = Genr_base.construction_function_name name in
  let gen_function_ident = make_Constant gen_function_name in
  Apply (gen_function_ident, [List args])
;;

let is_known_and_listary g args =
  (try Check.is_listary_generator g with
   | Check.Error _  -> false) &&
  List.length args > 1
;;

let is_known_and_listary_name gname args =
  is_known_and_listary (mk_generator gname Location.none) args
;;

(** Creates an expression in Code.exp representing a generator with the
    specified name and arguments *)
let mk_construction name args =
  let g = mk_generator name Location.none in
  if is_known_and_listary g args
  then mk_listary_construction name args
  else mk_non_listary_construction name args
;;

(** Creates an expression representing the equality between two arguments *)
let mk_equality e1 e2 = infix_name e1 "=" e2;;

let choose_ints rand n =
  if rand then Randomutils.ints n (n * 5)
  else Listutils.from_to 1 n succ
;;

(** Creates a number of Code.expressions representing [n] distinct constants. *)
let genr_constants_from_ints mk rand n = List.map mk (choose_ints rand n)
;;

let unit_of_int _n = ();;

let genr_ints = genr_constants_from_ints mk_int
and genr_chars = genr_constants_from_ints (fun n -> mk_char (char_of_int n))
and genr_strings = genr_constants_from_ints (fun n -> mk_string (string_of_int n))
and genr_floats = genr_constants_from_ints (fun n -> mk_float (float_of_int n))
and genr_int32s = genr_constants_from_ints (fun n -> mk_int32 (Int32.of_int n))
and genr_int64s = genr_constants_from_ints (fun n -> mk_int64 (Int64.of_int n))
and genr_nativeints =
  genr_constants_from_ints (fun n -> mk_nativeint (Nativeint.of_int n))
and genr_units =
  genr_constants_from_ints (fun n -> mk_unit (unit_of_int n))
(*and genr_units n = Listutils.take n [mk_tuple []]*)
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
