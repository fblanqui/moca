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

(* $Id: genr_pattern.ml,v 1.24 2012-04-02 09:27:25 weis Exp $ *)

open Parsetree
;;

let pat_underscore = {
  ppat_desc = Ppat_any;
  ppat_loc = Location.none;
}
;;

let remove_topconstr r =
  match r.ppat_desc with
  | Ppat_construct (_, Some p, _) -> p
  | Ppat_construct (_, None, _) -> r
  | _ -> assert false
;;

(*
let rec elim_parens pat =
  match pat.ppat_desc with
  | Ppat_parens pat -> elim_parens pat
  | _ -> pat
;;

(* Add an underscore for each pattern argument of a functional constructor. *)
let rec add_underscores pat =
  { pat with ppat_desc = add_underscores_aux pat.ppat_desc }

and add_opt = function
  | Some pat -> Some (add_underscores pat)
  | None -> None

and add_underscores_aux = function
  | Ppat_parens pat -> Ppat_parens (add_underscores pat)
  | Ppat_lazy pat -> Ppat_lazy (add_underscores pat)
  | Ppat_any -> Ppat_any
  | Ppat_var _ as pat -> pat
  | Ppat_alias (pat, s) -> Ppat_alias (add_underscores pat, s)
  | Ppat_constant _ as pat -> pat
  | Ppat_tuple pats -> Ppat_tuple (List.map add_underscores pats)

  | Ppat_construct (_gid, None, _b) as pat -> pat
  | Ppat_construct (gid, Some pat, b) when Check.is_gident gid ->
    let args =
      match (elim_parens pat).ppat_desc with
      | Ppat_tuple pats -> pats
      | _ -> [pat] in
    let args =
      { ppat_desc = Ppat_any; ppat_loc = Location.none } ::
      List.map add_underscores args in
    Ppat_construct (gid, Some { pat with ppat_desc = Ppat_tuple args }, b)
  | Ppat_construct (gid, Some pat, b) ->
    Ppat_construct (gid, Some (add_underscores pat), b)

  | Ppat_variant (label, popt) -> Ppat_variant (label, add_opt popt)
  | Ppat_record fields ->
    let add_underscores_to_field (label, pat) = (label, add_underscores pat) in
    Ppat_record (List.map add_underscores_to_field fields)
  | Ppat_array pats ->
    Ppat_array (List.map add_underscores pats)
  | Ppat_or (pat1, pat2) ->
    Ppat_or (add_underscores pat1, add_underscores pat2)
  | Ppat_constraint (pat, ct) ->
    Ppat_constraint (add_underscores pat, ct)
  | Ppat_type _ as pat -> pat
;;

*)

let rec add_underscores pat =
  match pat.ppat_desc with
  | Ppat_parens pat1 ->
    { pat with ppat_desc =
      Ppat_parens (add_underscores pat1) }
  | Ppat_lazy pat1 ->
    { pat with ppat_desc =
      Ppat_lazy (add_underscores pat1) }
  | Ppat_any -> pat
  | Ppat_var _ -> pat
  | Ppat_alias (pat1, s) ->
    { pat with ppat_desc =
      Ppat_alias (add_underscores pat1, s) }
  | Ppat_constant _ -> pat
  | Ppat_tuple pats ->
    { pat with ppat_desc =
      Ppat_tuple (List.map add_underscores pats) }

  | Ppat_construct (_, None, _) -> pat
  | Ppat_construct (gid, Some parg, b) when Check.is_generator_ident gid ->
    let rec add_underscore_to_arg parg =
      match parg.ppat_desc with
      | Ppat_tuple pats ->
        { parg with ppat_desc =
          Ppat_tuple (pat_underscore :: List.map add_underscores pats) }
      | Ppat_parens pat1 ->
        { parg with ppat_desc =
          Ppat_parens (add_underscore_to_arg pat1) }
      | _ ->
        { parg with ppat_desc =
          Ppat_tuple [pat_underscore; parg] } in
    { pat with ppat_desc =
      Ppat_construct (gid, Some (add_underscore_to_arg parg), b) }
  | Ppat_construct (gid, Some pat1, b) ->
    { pat with ppat_desc =
      Ppat_construct (gid, Some (add_underscores pat1), b) }

  | Ppat_variant (_, None) -> pat
  | Ppat_variant (label, Some pat1) ->
    { pat with ppat_desc =
      Ppat_variant (label, Some (add_underscores pat1)) }
  | Ppat_record (fields, eflag) ->
    let add_underscores_to_field (label, pat) =
      (label, add_underscores pat) in
    { pat with ppat_desc =
      Ppat_record (List.map add_underscores_to_field fields, eflag) }
  | Ppat_array pats ->
    { pat with ppat_desc =
      Ppat_array (List.map add_underscores pats) }
  | Ppat_or (pat1, pat2) ->
    { pat with ppat_desc =
      Ppat_or (add_underscores pat1, add_underscores pat2) }
  | Ppat_constraint (pat1, ct) ->
    { pat with ppat_desc =
      Ppat_constraint (add_underscores pat1, ct) }
  | Ppat_type _ -> pat
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
