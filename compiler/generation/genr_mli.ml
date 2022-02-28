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

(* $Id: genr_mli.ml,v 1.28 2012-04-02 09:27:25 weis Exp $ *)

open Pr_ocaml
open Format
open Parsetree
;;

let sharing_type_declaration sharing td =
  let commented_rels = Otype.comment_relations_in_type_declaration td in
  if sharing then
    Genr_sharing.add_info_arg_to_type_declaration commented_rels
  else commented_rels
;;

let sharing_type_declarations sharing =
  List.map (fun (s, td) -> s, sharing_type_declaration sharing td)
;;

let make_public_sharing_type_declarations sharing =
  List.map
    (fun (s, td) ->
     s, Otype.make_public_type_declaration (sharing_type_declaration sharing td))
;;

let genr_private_type_declarations sharing ppf ntds =
  Pr_ocaml.pr_type_definitions ppf (sharing_type_declarations sharing ntds);
;;

let genr_public_type_declarations sharing ppf ntds =
  Pr_ocaml.pr_type_definitions ppf
    (make_public_sharing_type_declarations sharing ntds);
  Genr_sharing.print_type_private_structure_items ppf ntds;
;;

let pr_type ppf (tname, td) =
  Pr_ocaml.pr_type ppf (tname, td.ptype_params)
;;

let genr_construction_function_interface ntd ppf (cname, cts, _rels, _loc) =
  match cts with
  | [] ->
    fprintf ppf "@[val@ %s :@ %a@]"
      (Genr_base.construction_function_name cname)
      pr_type ntd
  | _ :: _ ->
    fprintf ppf "@[val@ %s :@ %a ->@ %a@]"
      (Genr_base.construction_function_name cname)
      (pr_sep_vals " *" pr_core_type) cts
      pr_type ntd
;;

let record_construction_function_name tname = "make_" ^ tname
;;

let abbrev_construction_function_name = record_construction_function_name
;;

let abbrev_projection_function_name tname = "from_" ^ tname
;;

let genr_type_interface ppf ((tname, td) as ntd) =
  match td.ptype_kind, td.ptype_private with
  | Ptype_variant cdefs, Asttypes.Private ->
    pr_sep_vals_fmt "@;;;@;@;"
      (genr_construction_function_interface ntd) ppf cdefs;
      pr_structure_sep ppf
  | Ptype_record fdefs, Asttypes.Private ->
    let ctypes = List.map (fun (_label, _mut_flag, ctype, _rels, _loc) -> ctype) fdefs in
    fprintf ppf "@[val@ %s :@ %a ->@ %a@]"
      (record_construction_function_name tname)
      (pr_sep_vals  "*" pr_core_type) ctypes
      pr_type ntd;
    pr_structure_sep ppf
  | Ptype_abstract _rels, Asttypes.Private ->
    begin match td.ptype_manifest with
    | None -> ()
    | Some ctrep ->
      fprintf ppf "@[val@ %s :@ %a ->@ %a@]"
        (abbrev_construction_function_name tname)
        pr_core_type ctrep
        pr_type ntd;
      fprintf ppf "@;;;@;@;";
      fprintf ppf "@[external@ %s :@ %a ->@ %a =@ \"%%identity\"@]"
        (abbrev_projection_function_name tname)
        pr_type ntd
        pr_core_type ctrep;
      pr_structure_sep ppf;
    end
  | Ptype_variant _, Asttypes.Public
  | Ptype_abstract _, Asttypes.Public
  | Ptype_record _, Asttypes.Public -> ()
;;

let genr_eq_interface ppf ((tname, _td) as ntd) =
  fprintf ppf "@[val@ eq_%s :@ %a ->@ %a ->@ bool@]@;;;@;@;"
    tname
    pr_type ntd pr_type ntd
;;

let genr_type_interfaces ppf ntds =
  List.iter (genr_type_interface ppf) ntds
;;

let genr_eq_interfaces ppf ntds =
  let ntds =
    List.filter
      (fun (_, tdecl) -> Otype.is_private_variant_type_declaration tdecl)
      ntds in
  List.iter (genr_eq_interface ppf) ntds
;;

let genr_mli sharing ppf ntds =
  let genr_private_type_declarations =
    genr_private_type_declarations sharing in
  fprintf ppf "@[<v>%a%a%a@]"
    genr_private_type_declarations ntds
    genr_type_interfaces ntds
    genr_eq_interfaces ntds

;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
