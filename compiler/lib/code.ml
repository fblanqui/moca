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

(* $Id: code.ml,v 1.22 2012-06-04 13:01:22 weis Exp $ *)

(** {3 The code we generate.} *)

open Parsetree
open Longident
open Asttypes
;;

type lident = Longident.t
;;
(** Lowercase ident. *)
type uident = Longident.t
;;
(** Uppercase ident. *)
type iident = Longident.t
;;
(** Infix ident. *)
type ident = Longident.t
;;
(** Any kind of ident. *)

type iname = string
;;
(** The name of an infix symbole. *)
type tname = string
;;
(** The name of a type declared in the AST. *)
type filename = string
;;

type exp =
   | Constant of ident
   | Var of lident
   | Match of exp * clauses
   | Try of exp * clauses
   | Generator of (* FIXME Parsetree.generator *) uident * exp
   | Apply of exp * exp list
   | Infix of exp * iident * exp
   | If of exp * exp * exp
   | Let of lident * pattern list * exp * exp
   | Letrec of lident * pattern list * exp * exp
   | Sequence of exp * exp * exp list
   | Record of (lident * exp) list
   | Tuple of exp list
   | List of exp list
   | Ocaml_pattern of Parsetree.pattern
   | Ocaml_expression of Parsetree.expression
   | Function of clauses
   | Sline_directive of int * filename * exp
   | Comment of string * exp

and pattern = exp

and clause =
  | Clause of pattern * exp
  | Clause_when of pattern * exp * exp

and clauses = clause list
;;

type structure_item =
   (** Evaluates an expression *)
   | Pstr_eval of exp
   (** Defines a set of possibly recursive functional values *)
   | Pstr_value of Asttypes.rec_flag * (lident * exp list * exp) list
   (** Defines one primitive *)
   | Pstr_primitive of string * Parsetree.value_description
   (** Defines one type *)
   | Pstr_type of (string * Parsetree.type_declaration) list
   (** Defines one exception *)
   | Pstr_exception of string * Parsetree.exception_declaration
   (** Opens a module *)
   | Pstr_open of Longident.t
   (** Includes a module *)
   | Pstr_include of Parsetree.module_expr
   (** Comment belonging to a structure item *)
   | Pstr_comment of string * structure_item
;;

type structure = structure_item list
;;

let topletrec def = Pstr_value (Recursive, def)
;;

(* To add a prefix to an ident. *)
let rec prefix_longident s = function
  | Lident id -> Lident (s ^ id)
  | Ldot (modid, id) -> Ldot (modid, s ^ id)
  | Lapply (modid1, modid2) -> Lapply (modid1, prefix_longident s modid2)
;;

(* Globally predefined identifiers. *)

let make_Constant s = Constant (Lident s)
;;

let underscore = Var (Lident "_")
;;

let make_Var s = Var (Lident ("moca_" ^ s))
;;

let x = make_Var "x"
and y = make_Var "y"
and z = make_Var "z"
and t = make_Var "t"
and u = make_Var "u"
;;

let xs = make_Var "xs"
;;

(* Replace all variables by underscore in a Caml pattern. *)

let rec underscored_ocaml_pattern p =
  { p with ppat_desc = underscored_ocaml_patdesc p.ppat_desc }

and underscored_ocaml_pattern_opt = function
  | None -> None
  | Some p -> Some (underscored_ocaml_pattern p)

and underscored_ocaml_patdesc = function
  | Ppat_var _ -> Ppat_any
  | Ppat_alias (p, _) ->
      Ppat_parens (underscored_ocaml_pattern p)
  | Ppat_tuple l ->
      Ppat_tuple (List.map underscored_ocaml_pattern l)
  | Ppat_construct (lid, popt, b) ->
      Ppat_construct (lid, underscored_ocaml_pattern_opt popt, b)
  | Ppat_variant (lb, popt) ->
      Ppat_variant (lb, underscored_ocaml_pattern_opt popt)
  | Ppat_record (l, cflag) ->
      Ppat_record
        (List.map (fun (lid, p) -> (lid, underscored_ocaml_pattern p)) l, cflag)
  | Ppat_array l ->
      Ppat_array (List.map underscored_ocaml_pattern l)
  | Ppat_or (p1, p2 ) ->
      Ppat_or (underscored_ocaml_pattern p1, underscored_ocaml_pattern p2)
  | Ppat_constraint (p, ct) ->
      Ppat_constraint (underscored_ocaml_pattern p, ct)
  | Ppat_parens p ->
      Ppat_parens (underscored_ocaml_pattern p)
  | Ppat_lazy p ->
      Ppat_lazy (underscored_ocaml_pattern p)
  | (Ppat_type _
  | Ppat_any
  | Ppat_constant _) as p -> p
;;

let rec underscored_pattern = function
  | Var _ -> underscore
  | Tuple l -> Tuple (List.map underscored_pattern l)
  | Generator (uid, exp) -> Generator (uid, underscored_pattern exp)
  | Ocaml_pattern p -> Ocaml_pattern (underscored_ocaml_pattern p)
  | Record l ->
      Record (List.map (fun (lid, e) -> (lid, underscored_pattern e)) l)
  | Comment (s, p) -> Comment (s, underscored_pattern p)
  (* identity on other expressions *)
  | (Ocaml_expression _ | Match _ | Try _ | Apply _
  | Infix _ | If _ | Let _ | Letrec _ | Sequence _
  | Function _ | Constant _ | List _ | Sline_directive _) as x -> x
;;

(* Replace in a non-when clause lhs pattern all variables by underscore. *)
let underscored_clause = function
  | Clause (l, r) -> Clause (underscored_pattern l, r)
  | Clause_when _ as x -> x
;;

(* [genr_args root n] returns the list of strings [root1; ...; rootn].

   Beware: do not modify the code to generate a mere (admittedly
   simpler) "x", since arguments generated by [genr_args] should not be
   "x" or "u" or "t" or "y" or "z": those variables are already used in
   the generating functions as ``global'' (i.e. toplevel) arguments of
   generated functions.  If genr_args would produce "x" for instance,
   we would have a name capture between a pattern of a clause and the
   argument of the generated functions.  To prevent this phenomenon,
   genr_args always adds an integer after the root prefix. Be cautious
   to preserve this invariant! *)

let genr_args root =
  let rec loop accu = function
    | 0 -> accu
    | n ->
      let e = make_Var (root ^ string_of_int n) in
      loop (e :: accu) (n - 1) in
  loop []
;;

(* [genr_numbered_args root n]
   returns the list of strings [1, make_Var root1; ...; n, make_Var rootn].
   Beware: same as the one for [genr_args]. *)

let genr_numbered_args root n =
  let rec loop accu = function
    | 0 -> accu
    | n ->
      let e = make_Var (root ^ string_of_int n) in
      loop ((n, e) :: accu) (n - 1) in
  loop [] n
;;

(* [genr_underscores n] returns a list of underscores of length [n]. *)
let genr_underscores n =
  let rec loop accu = function
    | 0 -> accu
    | n ->
      loop (underscore :: accu) (n - 1) in
  loop [] n
;;


(* Application macros. *)

let apply f xs = Apply (Constant f, xs)
;;

let apply1 f x = Apply (Constant f, [x])
and apply2 f x y = Apply (Constant f, [x; y])
and apply3 f x y z = Apply (Constant f, [x; y; z])
;;

let infix_apply x f y = Infix (x, f, y)
and infix_name x f y = Infix (x, Lident f, y)
;;

(* Other useful auxiliaries. *)

let let0 x v b = Let (x, [], v, b)
and let1 f x v b = Let (f, [x], v, b)
and pair x y = Tuple [x; y]
and raise_error s = apply1 (Lident "raise") (make_Constant s)
;;

let division_by_absorbent =
  make_Constant "\"Division by Absorbent\""
;;

let failure_division_by_absorbent =
  apply1 (Lident "Failure") division_by_absorbent
;;

let failwith_division_by_absorbent =
  apply1 (Lident "raise") failure_division_by_absorbent
;;

let sequence e1 e2 = Sequence (e1, e2, [])
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
