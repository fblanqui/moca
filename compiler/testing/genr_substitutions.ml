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

(* $Id: genr_substitutions.ml,v 1.12 2012-04-02 09:27:26 weis Exp $ *)

(** {Generation of substitutions mapping typed variables to values Code.exp.} *)

exception Not_enough_values
;;

(** Converts an association list [ (v1, t1); (v2; t2); ... ] to a function
    mapping each vi to ti. *)
let to_function assoc = fun x -> List.assoc x assoc
;;

(** Takes a list of association lists, each representing a group of n
    substitutions from variables to values of a certain type, and builds a
    list of n associations joining together the i_th substitution of each
    group.

    For example the list
    [
      [ assoc_bool1; assoc_bool2; assoc_bool3; ];
      [ assoc_int1; assoc_int2; assoc_int3; ];
      [ assoc_string1; assoc_string2; assoc_string3; ];
    ]
    becomes
    [
       assoc_bool1 @ assoc_int1 @ assoc_string1;
       assoc_bool2 @ assoc_int2 @ assoc_string2;
       assoc_bool3 @ assoc_int3 @ assoc_string3;
    ]
*)

let join_typed_assocs assocs =
  List.map List.flatten (Listutils.transpose assocs)
;;

let complete values n =
  let nvals = List.length values in
  let reps_needed = n / nvals in
  let nrest = n - nvals * reps_needed in
  let rest = Listutils.take nrest values in
  List.flatten (Listutils.repeat reps_needed values) @ rest
;;

let get_enough_values genr_values t n =
  let values = genr_values t n in
  if values = [] then raise Not_enough_values else
  if List.length values < n then complete values n else
  values
;;

(** Generates a list of n substitutions, i.e. functions mapping each
    variable to a value of type t. Uses specified function to generate a
    number of values of a certain type. *)
let genr_type_substs n_substs genr_values (t, vars) =
  let n_vars = List.length vars in
  let n_values = n_substs * n_vars in
  let values = get_enough_values genr_values t n_values in
  let genr_assoc i =
    let ith_values = Listutils.ith_group i n_vars values in
    List.combine vars ith_values in
  Listutils.map_from_to genr_assoc 0 (n_substs - 1) succ
;;

(** Generates a list of n substitutions, i.e. functions mapping each
    variable to a value of its corresponding type. Uses [genr_values] as
    a function that generates a specified number of values of a given type
*)

let has_type t1 (_, t2) = Otype.eq_type t1 t2
;;

let rec vars_by_type = function
  | [] -> []
  | (v, t) :: vts ->
    let (t_vars, non_t_vars) = List.partition (has_type t) vts in
    (t, v :: List.map fst t_vars) :: vars_by_type non_t_vars
;;

let genr_substs n_substs genr_values typed_vars =
  let vs_by_type = vars_by_type typed_vars in
  let assocs_by_type =
    List.map (genr_type_substs n_substs genr_values) vs_by_type in
  List.map to_function (join_typed_assocs assocs_by_type)
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
