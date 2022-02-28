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

(* $Id: pr_ocaml.ml,v 1.44 2012-06-04 13:01:22 weis Exp $ *)

open Format
open Longident
open Asttypes
open Parsetree
open Debug;;

(** {3 Printing Objective Caml types and expressions.} *)

(* Printing verbatim code *)
let find_text_in_file loc =
  let loc_start = loc.Location.loc_start in
  let fname, _, _ = Location.get_pos_info loc_start in
  let pos_cnum = loc_start.Lexing.pos_cnum in
  let pos_enum = loc.Location.loc_end.Lexing.pos_cnum in
  let ic =
    try open_in fname with
    | _ ->
      failwith
        (Printf.sprintf
           "find_text_in_file: file ``%s'' not found or not readable."
           fname) in
  let len = pos_enum - pos_cnum in
  let buff = String.create len in
  try
    seek_in ic pos_cnum;
    really_input ic buff 0 len;
    buff with
  | End_of_file ->
    failwith
      (Printf.sprintf
         "find_text_in_file: file ``%s'' is too short."
         fname)
;;

let pr_verbatim ppf loc = fprintf ppf "@[%s@]" (find_text_in_file loc);;

let pr_verbatim_structure_item ppf si = pr_verbatim ppf si.pstr_loc;;

(* Printing parens around a construct: we open the box before printing the
   open parens, so that ellipsis will print "(...)" and not "...". *)
let pr_parens ppf pr_x x =
  fprintf ppf
    "(@[<hv 0>\
      %a@,\
     @])"
    pr_x x
;;

let pr_begin_end ppf pr_x x =
  fprintf ppf
    "@[<hv 0>\
     @[<hv 2>\
     begin@ \
       %a\
     @]@ \
     end\
     @]"
    pr_x x
;;

(* Printing separation of structure items *)
let pr_structure_sep ppf = fprintf ppf "@;;;@;@;";;

let is_infix s =
  String.length s > 0 &&
  match s.[0] with
  | '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<'
  | '=' | '>' | '?' | '@' | '^' | '|' | '~' -> true
  | _ -> false
;;

let rec is_infix_longident = function
  | Lident s -> is_infix s
  | Ldot (_mod, s) -> is_infix s
  | Lapply (_funct, id) -> is_infix_longident id
;;

(* Printing identifiers *)
let rec pr_qualified_ident pr_ident ppf = function
  | Lident s ->  pr_ident ppf s
  | Ldot (t, s) ->
      fprintf ppf "%a.%a" (pr_qualified_ident pr_ident) t pr_ident s
  | Lapply (t1, t2) ->
      fprintf ppf "%a(%a)"
        (pr_qualified_ident pr_ident) t1
        (pr_qualified_ident pr_ident) t2
;;

(* Prints an unqualified (possibly infix) identifier
   in infix position. *)
let pr_ident_name_in_infix_position ppf = fprintf ppf "%s";;

(* Prints a possibly qualified (possibly infix) identifier
   in infix position. *)
let pr_ident_in_infix_position =
  pr_qualified_ident pr_ident_name_in_infix_position
;;

(* Prints an unqualified (possibly infix) identifier
   in prefix position. *)
let pr_ident_name_in_prefix_position ppf s =
  if is_infix s
  then fprintf ppf "( %s )" s
  else fprintf ppf "%s" s
;;

(* Prints a possibly qualified (possibly infix) identifier
   in prefix position. *)
let pr_ident_in_prefix_position =
  pr_qualified_ident pr_ident_name_in_prefix_position
;;

(* Printing constants *)
let pr_constant ppf = function
  | Const_int i ->
    if i < 0 then fprintf ppf "(%i)" i else fprintf ppf "%i" i
  | Const_char c -> fprintf ppf "%C" c
  | Const_string s -> fprintf ppf "%S" s
  | Const_float s -> fprintf ppf "%s" s
  | Const_int32 li -> fprintf ppf "%lil" li
  | Const_int64 lli -> fprintf ppf "%LiL" lli
  | Const_nativeint ni -> fprintf ppf "%nin" ni
;;

let string_of_constant =
  let b = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer b in
    fun c -> Buffer.clear b; pr_constant ppf c; Buffer.contents b
;;

(* Printing labels *)
let pr_label ppf = function
  | "" -> ()
  | l -> fprintf ppf ":%s" l
;;

(* Printing optional values if they exist *)
let option print_item ppf = function
  | Some x -> fprintf ppf "%a" print_item x
  | None -> ()
;;

(* Printing several values as a sequence *)
let pr_suffix_vals_fmt suffix print_item ppf =
  List.iter (fun e -> print_item ppf e; fprintf ppf suffix)
;;

(* Print a list of values [\[e1; ...; en\]]
   - the first item is printed,
   - then the function that prints a separator then an item is applied to the
     rest of the list of items.
   - nothing happens if the list is empty.
*)
let pr_sep_vals_fmt =

  let pr_prefix_vals_fmt prefix print_item ppf =
    List.iter
      (fun e ->
       fprintf ppf prefix;
       print_item ppf e) in

  fun separator print_item ppf -> function
  | [] -> ()
  | e :: es ->
    print_item ppf e;
    pr_prefix_vals_fmt separator print_item ppf es
;;

let pr_prefix_vals prefix print_item ppf =
  List.iter (fun e -> fprintf ppf "%s@ %a" prefix print_item e)
;;

let pr_space_vals print_item ppf = pr_prefix_vals "" print_item ppf
;;

let pr_suffix_vals suffix print_item ppf =
  List.iter (fun e -> fprintf ppf "%a%s@ " print_item e suffix)
;;

let pr_sep_vals separator print_item ppf = function
  | [] -> ()
  | e :: es ->
    fprintf ppf "@[<hov 0>%a@]" print_item e;
    pr_prefix_vals separator print_item ppf es
;;

(* FIXME Here: fix for a () problem in generated code,
   we added parens around all tuples. To be finer grain (use context to
   decide is parens are mandatory!). *)
let pr_as_tuple print_item ppf vs =
  fprintf ppf "@[<hov 0>%a@]" (pr_sep_vals "," print_item) vs
;;

let pr_verbatim_structure_items ppf sis =
  fprintf ppf "@[<v 0>%a@]"
    (pr_suffix_vals_fmt "@;;;@;@;" pr_verbatim_structure_item) sis
;;

let pr_application pr_fun_caller pr_fun_args ppf f args =
  fprintf ppf
    "@[<hv 2>\
     @[<hov 0>%a@]%a\
     @]"
    pr_fun_caller f
    pr_fun_args args
;;

(* Printing infix operations, constructions, arrays, records *)
let pr_infix_application pr_arg ppf e1 id e2 =
  fprintf ppf
    "@[<hv 0>\
     %a %a@ \
     %a\
     @]"
    pr_arg e1 pr_ident_in_infix_position id
    pr_arg e2
;;

(* Printing types. *)
let rec pr_core_type ppf x =
  match x.ptyp_desc with
  | Ptyp_any -> fprintf ppf "_"
  | Ptyp_var s -> fprintf ppf "'%s" s
  | Ptyp_arrow (_l, ct1, ct2) -> pr_arrow ppf ct1 ct2
  | Ptyp_tuple cts -> fprintf ppf "@[%a@]" pr_core_types cts
  | Ptyp_constr (li, cts) -> pr_type_constructor ppf li cts
  | Ptyp_variant _
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_poly _
  | Ptyp_package _ ->
      not_yet_implemented "Printing of variant, classes and aliases."
  | Ptyp_parens ct -> pr_parens ppf pr_core_type ct

and pr_arrow ppf t1 t2 =
  fprintf ppf "%a ->@ %a" pr_core_type t1 pr_core_type t2

and pr_type_constructor ppf li = function
  | [] -> pr_ident_in_prefix_position ppf li
  | [ct] ->
    fprintf ppf
      "@[<hv 0>\
       %a@ \
       %a\
       @]"
      pr_core_type ct
      pr_ident_in_prefix_position li
  | cts ->
    fprintf ppf
      "@[<hv 0>\
       (%a)@ \
       %a\
       @]"
      (pr_as_tuple pr_core_type) cts
      pr_ident_in_prefix_position li

and pr_core_types ppf cts = pr_sep_vals " *" pr_core_type ppf cts
;;

let pr_constructor_type_arguments ppf = function
  | [] -> ()
  | args -> fprintf ppf " of@ %a" pr_core_types args
;;

(* Printing special syntax for lists by extension *)
let ident_list_nil = Longident.Lident "[]"
let ident_list_cons = Longident.Lident "::"
;;

let is_cons_ident lid = lid = ident_list_cons
;;

let expr_list_by_extension e =
  let rec accum_list_by_extension accum e =
    match e.pexp_desc with
    | Pexp_ident lid
        when lid = ident_list_nil ->
      Some (List.rev accum)
    | Pexp_construct (id, None, _)
        when id = ident_list_nil ->
      Some (List.rev accum)
    | Pexp_apply (({ pexp_desc = Pexp_ident id; pexp_loc = _ }), [ "", e1; "", e2; ])
        when id = ident_list_cons ->
      accum_list_by_extension (e1 :: accum) e2
    | Pexp_construct (id, Some { pexp_desc = Pexp_tuple [ e1; e2; ];
                                 pexp_loc = _ }, _)
        when id = ident_list_cons ->
      accum_list_by_extension (e1 :: accum) e2
    | Pexp_parens e -> accum_list_by_extension accum e
    | Pexp_begend _ | Pexp_ident _ | Pexp_constant _
    | Pexp_let _ | Pexp_function _ | Pexp_apply _ | Pexp_match _
    | Pexp_try _ | Pexp_tuple _ | Pexp_construct _ | Pexp_variant _
    | Pexp_record _ | Pexp_field _ | Pexp_setfield _ | Pexp_array _
    | Pexp_ifthenelse _ | Pexp_sequence _ | Pexp_while _ | Pexp_for _
    | Pexp_constraint _ | Pexp_when _ | Pexp_send _ | Pexp_new _
    | Pexp_setinstvar _ | Pexp_override _ | Pexp_letmodule _
    | Pexp_assert _ | Pexp_assertfalse | Pexp_lazy _ | Pexp_poly _
    | Pexp_object _
    | Pexp_open (_, _) | Pexp_pack (_, _) | Pexp_newtype (_, _) -> None in
  accum_list_by_extension [] e
;;

let pat_list_by_extension p =
  let rec accum_list_by_extension accum p =
    match p.ppat_desc with
    | Ppat_construct (id, None, _)
       when id = ident_list_nil ->
      Some (List.rev accum)
    | Ppat_construct (id, Some { ppat_desc = Ppat_tuple [ p1; p2; ];
                                 ppat_loc = _ }, _)
        when id = ident_list_cons ->
      accum_list_by_extension (p1 :: accum) p2
    | Ppat_any | Ppat_var _ | Ppat_alias _ | Ppat_constant _ | Ppat_tuple _
    | Ppat_construct _ | Ppat_variant _ | Ppat_record (_, _) | Ppat_array _
    | Ppat_or _ | Ppat_constraint _ | Ppat_type _
    | Ppat_lazy _ | Ppat_parens _ -> None in
  accum_list_by_extension [] p
;;

let pr_list_by_extension print_item ppf es =
  fprintf ppf "[ @[<hov 0>%a@] ]" (pr_sep_vals ";" print_item) es

let pr_list_cons print_item build_list ppf e1 id e2 =
  match build_list e2 with
  | None -> pr_infix_application print_item ppf e1 id e2
  | Some es -> pr_list_by_extension print_item ppf (e1 :: es)
;;

let pr_as_infix print_item build_list ppf e1 id e2 =
  if is_cons_ident id
  then pr_list_cons print_item build_list ppf e1 id e2
  else pr_infix_application print_item ppf e1 id e2
;;

let pr_field print_item ppf (lbl_id, e) =
  fprintf ppf "%a =@ %a" pr_ident_in_prefix_position lbl_id print_item e
;;

let pr_fields print_item ppf fds = pr_sep_vals ";" (pr_field print_item) ppf fds
;;

let pr_as_record print_item ppf fds with_opt =
  let pr_with ppf e = fprintf ppf "%a@ with" print_item e in
  fprintf ppf "{ @[<hov 0>%a%a@] }" (option pr_with) with_opt (pr_fields print_item) fds
;;

let pr_as_array print_item ppf vs =
  fprintf ppf "[| @[<hov 0>%a@] |]" (pr_suffix_vals ";" print_item) vs
;;

let pr_as_variant print_item ppf lbl vopt = fprintf ppf "`%s@ %a" lbl (option print_item) vopt
;;

let pr_as_constraint print_item ppf e ctopt1 ctopt2 =
  let pr_ct1 ppf t = fprintf ppf " :@ %a" pr_core_type t
  and pr_ct2 ppf t = fprintf ppf " :>@ %a" pr_core_type t in
  fprintf ppf "%a%a%a" print_item e (option pr_ct1) ctopt1
    (option pr_ct2) ctopt2
;;

(* Printing patterns. *)
let rec pr_pattern ppf { ppat_desc = p; ppat_loc = _ } =
  match p with
  | Ppat_any -> fprintf ppf "_"
  | Ppat_var s -> fprintf ppf "%s" s
  | Ppat_alias (p, s) ->
      fprintf ppf
        "@[<hv 0>\
         %a@ \
         as %a\
         @]"
        pr_pattern p
        pr_ident_name_in_prefix_position s
  | Ppat_constant c -> pr_constant ppf c
  | Ppat_tuple [] -> assert false
  | Ppat_tuple [ _ ] -> assert false
  | Ppat_tuple pats -> pr_as_tuple pr_pattern ppf pats
  | Ppat_construct (id, None, _) -> pr_ident_in_prefix_position ppf id
  | Ppat_construct
      (id,
       Some { ppat_desc = Ppat_tuple [ p1; p2; ]; ppat_loc = _ },
       _) when is_infix_longident id ->
    pr_as_infix pr_pattern pat_list_by_extension ppf p1 id p2
  | Ppat_construct (id, Some pat, _) ->
(* Fixme: should not add parens in all circumstances! *)
    pr_application pr_ident_in_prefix_position
      (fun ppf pat -> fprintf ppf "@ @[<hov 0>(%a)@]" pr_pattern pat)
      ppf id pat
  | Ppat_variant (lbl, popt) -> pr_as_variant pr_pattern ppf lbl popt
  | Ppat_record (idpats, _) -> pr_as_record pr_pattern ppf idpats None
  | Ppat_array pats -> pr_as_array pr_pattern ppf pats
  | Ppat_or (p1, p2) -> fprintf ppf "@[<hv 0>%a |@ %a@]" pr_pattern p1 pr_pattern p2
  | Ppat_constraint (p, ct) ->
    pr_as_constraint pr_pattern ppf p (Some ct) None
  | Ppat_type id -> fprintf ppf "#%a" pr_ident_in_prefix_position id
  | Ppat_lazy p -> fprintf ppf "@[<hov 2>lazy@ %a@]" pr_pattern p
  | Ppat_parens p -> pr_parens ppf pr_pattern p
;;

let print_pattern ppf pat =
  fprintf ppf
    "@[<hov 0>%a@]"
    pr_pattern pat
;;

(* Printing value descriptiosn *)
let pr_value_description ppf x =
  fprintf ppf " :@ %a" pr_core_type x.pval_type;
  match x.pval_prim with
  | [] -> ()
  | l ->
    fprintf ppf " =@;%a" (pr_space_vals (fun ppf -> fprintf ppf "%S")) l
;;


(* Printing module and type definitions *)
let pr_param ppf p = fprintf ppf "'%s" p
;;

let pr_params ppf = function
  | [] -> ()
  | [param] -> fprintf ppf "%a@ " pr_param param
  | params -> fprintf ppf "(%a)@ " (pr_as_tuple pr_param) params
;;

let rec pr_type ppf (ty_name, params) =
  fprintf ppf "%a%s" pr_params params ty_name
;;

let pr_private_flag ppf = function
  | Public -> ()
  | Private -> fprintf ppf " private"
;;

let pr_mutable_flag ppf = function
  | Immutable -> ()
  | Mutable -> fprintf ppf " mutable "
;;

let pr_side ppf = function
  | Right -> fprintf ppf " right"
  | Left -> fprintf ppf " left"
  | Both -> ()

and pr_side_if_left ppf = function
  | Left -> fprintf ppf " left"
  | Right
  | Both -> ()

and pr_status ppf = function
  | Lexicographic -> fprintf ppf "lexicographic"
  | Multiset -> fprintf ppf "multiset"

and pr_inverse_opt ppf = function
  | Dist_Inverse -> fprintf ppf " inverse"
  | Dist_Direct -> ()

and pr_generator ppf g = pr_ident_in_prefix_position ppf g.pgen_desc
;;

(*
let pr_type_manifest ppf ct = fprintf ppf " =@ %a" pr_core_type ct
;;
*)

let rec pr_relation ppf rel =
  match rel.prel_desc with
  | Rewrite (pat, expr) ->
    (match expr.pexp_desc with
     | Pexp_when (e1, e2) ->
       fprintf ppf
         "@[<hv 2>\
          rule @[<hov 0>%a@]@ \
            when @[<hov 0>%a@] ->@ \
            @[<hov 0>%a@]\
          @]"
         pr_pattern pat
         pr_expression e1
         pr_expression e2
     | _ ->
       fprintf ppf
         "@[<hv 2>rule @[<hov 0>%a@] ->@ \
            @[<hov 0>%a@]\
          @]"
         pr_pattern pat
         pr_expression expr)
  | Commutative None -> fprintf ppf "commutative"
  | Commutative (Some id) ->
    fprintf ppf "commutative (%a)" pr_ident_in_prefix_position id
  | Associative _sd -> fprintf ppf "associative" (*pr_side_if_left sd*)
  | Absorbent (sd, g) ->
    fprintf ppf "absorbent%a (%a)" pr_side_if_left sd pr_generator g
  | Absorbing (sd, g) ->
    fprintf ppf "absorbing%a (%a)" pr_side sd pr_generator g
  | Neutral (sd, g) ->
    fprintf ppf "neutral%a (%a)" pr_side sd pr_generator g
  | Idempotent sd ->
    fprintf ppf "idempotent%a" pr_side sd
  | Nilpotent (sd, g) ->
    fprintf ppf "nilpotent%a (%a)"
      pr_side sd
      pr_generator g
  | Involutive ->
    fprintf ppf "involutive"
  | Inverse (sd, g, None) ->
    fprintf ppf "inverse%a (%a)" pr_side sd pr_generator g
  | Inverse (sd, g1, Some g2) ->
    fprintf ppf "inverse%a (%a, %a)" pr_side sd
      pr_generator g1 pr_generator g2
  | Distributive (sd, g, None, b) ->
    fprintf ppf "distributive%a%a (%a)"
      pr_inverse_opt b pr_side sd pr_generator g
  | Distributive (sd, g, Some h, b) ->
    fprintf ppf "distributive%a%a (%a, %a)"
      pr_inverse_opt b pr_side sd pr_generator g pr_generator h
  | Division_by_Absorbent _ -> fprintf ppf "division by absorbent"
  | Precedence n -> fprintf ppf "completion precedence %i" n
  | Status s -> fprintf ppf "completion status %a" pr_status s
  | Structure_item struct_item ->
    fprintf ppf "@[%a@;;;@]" pr_structure_item struct_item

(* Printing Moca relations *)
and pr_relations ppf rels =
  let rec do_pr_relations ppf rels =
    match rels.prels_desc with
    | Prels_none -> ()
    | Prels_commented { prels_desc = Prels_none | Prels_begend [];
                        prels_loc = _ } -> ()
    | Prels_commented rels -> fprintf ppf "(* %a *)" do_pr_relations rels
    | Prels_begend rels ->
        fprintf ppf
          "@[<v 0>\
           @[<v 2>\
           begin@ \
             %a\
           @]@ \
           end@]"
          (pr_sep_vals_fmt "@ " pr_relation) rels in
  match rels.prels_desc with
  | Prels_none
  | Prels_commented { prels_desc = Prels_none | Prels_begend [];
                      prels_loc = _ } -> ()
  | Prels_commented _ | Prels_begend _ ->
      fprintf ppf "@ %a" do_pr_relations rels

and pr_type_kind ppf (kind, private_flag, ctype_opt) =
  match kind, private_flag, ctype_opt with
    (* What to do with rels ? *)
  | Ptype_abstract _rels, Public, None -> ()
  | Ptype_abstract rels, Private, None ->
      fprintf ppf
        " = private@ \
        %a"
        pr_relations rels
  | Ptype_abstract rels, Public, Some cty ->
      fprintf ppf
        " =@ \
        %a%a"
        pr_core_type cty
        pr_relations rels
  | Ptype_abstract rels, Private, Some cty ->
      fprintf ppf
        " = private@ \
        %a%a"
        pr_core_type cty
        pr_relations rels
  | Ptype_variant cdefs, private_flag, None ->
      fprintf ppf
        " =%a@ \
        %a"
        pr_private_flag private_flag
        pr_constructor_definitions cdefs
  | Ptype_variant cdefs, private_flag, Some cty ->
      fprintf ppf
        " = %a =%a@ \
        %a"
        pr_core_type cty
        pr_private_flag private_flag
        pr_constructor_definitions cdefs
  | Ptype_record fdefs, private_flag, None ->
      fprintf ppf
        " =%a@ \
        %a"
        pr_private_flag private_flag
        pr_label_definitions fdefs
  | Ptype_record fdefs, private_flag, Some cty ->
      fprintf ppf
        " = %a = %a@ \
        %a"
        pr_core_type cty
        pr_private_flag private_flag
        pr_label_definitions fdefs

and pr_type_binding_gen kwd ppf (ty_name, ty_def) =
  fprintf ppf
    "@[<v 0>\
     @[<hv %i>\
     %s %a%a\
     @]\
     @]"
    3
    kwd
    pr_type (ty_name, ty_def.ptype_params)
    (* Skip variance *)
    (* Skip constraints *)
    pr_type_kind
      (ty_def.ptype_kind, ty_def.ptype_private, ty_def.ptype_manifest)

and pr_other_type_binding ppf =
  pr_type_binding_gen "and" ppf
and pr_type_binding ppf =
  pr_type_binding_gen "type" ppf

and pr_other_type_bindings ppf ty_bdgs =
  List.iter
    (fun ty_bdg ->
     fprintf ppf
       "@ %a"
       pr_other_type_binding ty_bdg)
    ty_bdgs

and pr_type_definitions ppf ty_bdgs =
  match ty_bdgs with
  | [] -> assert false (* No definition. *)
  | first_ty_bdg :: other_ty_bdgs ->
    fprintf ppf
      "@[<v 0>\
       %a%a\
       @ \
       ;;@ \
       @]"
      pr_type_binding first_ty_bdg
      pr_other_type_bindings other_ty_bdgs

and pr_module_binding ppf mb =
  match mb.pmod_desc with
  | Pmod_constraint (me, mty) ->
      fprintf ppf "@[:@ %a =@ %a@]" pr_module_type mty pr_module_expr me
  | Pmod_functor (arg_name, mty, me) ->
      fprintf ppf
        "@[<hv 2>\
         (@[<hv 2>\
          %s :@ \
            %a@])@ \
           %a\
         @]"
        arg_name
        pr_module_type mty
        pr_module_expr me
  | _ ->
      fprintf ppf
        "= %a"
        pr_module_expr mb

and pr_module_type ppf mt =
  match mt.pmty_desc with
  | Pmty_ident mty_ident ->
      fprintf ppf "%a" pr_ident_in_prefix_position mty_ident
  | Pmty_signature l ->
      fprintf ppf
        "@[<hv 2>\
         sig@ \
           %a\
         @]@ \
         end"
        pr_signature l
  | Pmty_functor (arg_name, mty1, mty2) ->
      fprintf ppf
        "@[<hv 2>\
         functor@ \
           @[<hv 2>\
           (%s :@ \
             @[\
             %a\
             @])\
           @] ->@ \
           @[\
           %a\
           @]\
         @]"
        arg_name
        pr_module_type mty1
        pr_module_type mty2
  | Pmty_with (mty, clist) ->
      fprintf ppf
        "@[<hv 2>\
         @[\
         %a\
         @]@ \
           @[<hv 2>\
           with@ \
             %a\
           @]\
         @]"
        pr_module_type mty
        pr_constraints clist
  | Pmty_typeof mexpr ->
      fprintf ppf
        "@[<hv 2>\
         module type of@ \
           %a\
         @]"
        pr_module_expr mexpr
  | Pmty_parens mty -> pr_parens ppf pr_module_type mty

and pr_module_expr ppf mexpr =
  match mexpr.pmod_desc with
  | Pmod_ident uid ->
      fprintf ppf "%a" pr_ident_in_prefix_position uid
  | Pmod_structure stru ->
      fprintf ppf
        "struct@ \
         @[<hv 0>\
         %a\
         @]@ \
         end"
        pr_structure stru
  | Pmod_functor (arg_name, mty, mexpr) ->
      fprintf ppf
        "@[<hv 0>\
         functor @[<hv 1>(%s :@ %a)@] ->@ \
         @[%a@]\
         @]"
        arg_name
        pr_module_type mty
        pr_module_expr mexpr
  | Pmod_apply (mexpr1, mexpr2) ->
      fprintf ppf
        "@[<hv 2>\
         @[\
         %a\
         @]@ \
           @[<hv 1>(%a)@]\
         @]"
        pr_module_expr mexpr1
        pr_module_expr mexpr2
  | Pmod_constraint (mexpr, mty) ->
      fprintf ppf
        "@[<hv 1>\
         (@[\
          %a\
          @] :@ \
          @[\
          %a\
          @])\
         @]"
        pr_module_expr mexpr
        pr_module_type mty
  | Pmod_unpack (expr, pty) ->
      fprintf ppf
        "@[<hv 1>\
         (@[\
          val %a\
          @] :@ \
          @[\
          %a\
          @])\
         @]"
        pr_expression expr
        pr_package_type pty
  | Pmod_parens mexpr -> pr_parens ppf pr_module_expr mexpr

and pr_package_type ppf = function
  | mty_id, [] -> fprintf ppf "%a" pr_ident_in_prefix_position mty_id
  | mty_id, ptype_cstrs ->
    fprintf ppf "@[<hv 2>%a with@ %a@]"
      pr_ident_in_prefix_position mty_id pr_package_type_cstrs ptype_cstrs

and pr_package_type_cstr ppf (id, ctype) =
  fprintf ppf "type %s = %a" id pr_core_type ctype

and pr_package_type_cstrs ppf ptype_cstrs =
  fprintf ppf "@[<hv>%a@]"
    (pr_sep_vals_fmt "@ " pr_package_type_cstr) ptype_cstrs

and pr_constructor_definition ppf (cname, ctys, rels, _loc) =
  fprintf ppf
    "@[\
     %a%a\
     @]\
     %a"
    pr_ident_name_in_prefix_position cname
    pr_constructor_type_arguments ctys
    pr_relations rels

and pr_constructor_definitions ppf cdefs =
  let pr_cdef ppf cdef = fprintf ppf "@[<v 0>| %a@]" pr_constructor_definition cdef in
  pr_sep_vals_fmt "@ " pr_cdef ppf cdefs

and pr_label_definition ppf (lbl, mflag, cty, rels, _loc) =
  fprintf ppf "@[%a%a :@ %a%a@]"
    pr_mutable_flag mflag
    pr_ident_name_in_prefix_position lbl
    pr_core_type cty
    pr_relations rels

and pr_label_definitions ppf lbds =
  fprintf ppf "@[<hv 0>%a@]"
    (pr_suffix_vals_fmt ";@ " pr_label_definition) lbds

and pr_primitive_body_definition ppf defs =
  pr_sep_vals_fmt "@ " (fun ppf def -> fprintf ppf "%S" def) ppf defs

and pr_primitive_definition ppf name ty defs =
  fprintf ppf
    "@[<hv 2>external %a@ \
       : @[<hov 0>%a@]@ \
       = @[<hov 0>%a@]\
     @]"
    pr_ident_name_in_prefix_position name
    pr_core_type ty
    pr_primitive_body_definition defs

and pr_constraints _ob _cl =
  not_yet_implemented "Printing of constraints."

(* TODO : print class structures*)
and pr_class_struct _ob _c =
  not_yet_implemented "Printing of class_struct."

and print_expression ppf e =
  fprintf ppf
   "@[<hov 0>%a@]"
   pr_expression e

(* Printing expressions *)
and pr_expression ppf e =
  match e.pexp_desc with
  | Pexp_parens e -> pr_parens ppf pr_expression e
  | Pexp_begend e -> pr_begin_end ppf pr_expression e
  | Pexp_ident lid -> fprintf ppf "%a" pr_ident_in_prefix_position lid
  | Pexp_constant c -> fprintf ppf "%a" pr_constant c
  | Pexp_let (rf, pel, e) -> pr_let_in ppf rf pel e
  | Pexp_function (l, eopt, pel) -> pr_function ppf l eopt pel
  | Pexp_apply ({ pexp_desc = Pexp_ident (Lident s); pexp_loc = _ }, [ "", arg1; "", arg2 ])
      when is_infix s -> pr_infix ppf arg1 s arg2
  | Pexp_apply (e, les) -> pr_apply ppf e les
  | Pexp_match (e, pel) -> pr_match ppf "match" e pel
  | Pexp_try (e, pel) -> pr_match ppf "try" e pel
  | Pexp_tuple el -> pr_tuple ppf el
  | Pexp_construct (lid, None, _) -> pr_ident_in_prefix_position ppf lid
  | Pexp_construct (id, Some { pexp_desc = Pexp_tuple [ e1; e2; ];
                               pexp_loc = _ }, _)
      when is_infix_longident id ->
      pr_as_infix print_expression expr_list_by_extension ppf e1 id e2
  | Pexp_construct (lid, Some e, _) ->
      fprintf ppf "%a %a" pr_ident_in_prefix_position lid pr_expression e
  | Pexp_variant (l, eopt) -> pr_as_variant pr_expression ppf l eopt
  | Pexp_record (les, with_opt) -> pr_record ppf les with_opt
  | Pexp_field (e, lid) ->
      fprintf ppf "%a.%a" pr_ident_in_prefix_position lid pr_expression e
  | Pexp_setfield (r, lbl, e) -> pr_setfield ppf r lbl e
  | Pexp_array exprs -> pr_as_array pr_expression ppf exprs
  | Pexp_ifthenelse (cond, then_expr, else_opt) ->
      pr_if ppf cond then_expr else_opt
  | Pexp_sequence (expr1, expr2) -> pr_sequence ppf expr1 expr2
  | Pexp_while (expr1, expr2) -> pr_while ppf expr1 expr2
  | Pexp_for (idx, expr1, expr2, dir_flag, expr3) ->
      pr_for ppf idx expr1 expr2 dir_flag expr3
  | Pexp_constraint (expr, ct_option1, ct_option2) ->
      pr_as_constraint pr_expression ppf expr ct_option1 ct_option2
  | Pexp_when _ -> assert false
  | Pexp_send (e, s) -> pr_send ppf e s
  | Pexp_new t -> fprintf ppf "new@ %a" pr_ident_in_prefix_position t
  | Pexp_setinstvar (s, e) -> pr_setinsvar ppf s e
  | Pexp_override l -> pr_override ppf l
  | Pexp_letmodule (s, m, e) -> pr_let_module_in ppf s m e
  | Pexp_assert e -> fprintf ppf "assert@ %a" pr_expression e
  | Pexp_assertfalse -> fprintf ppf "assert false"
  | Pexp_lazy e -> fprintf ppf "lazy@ %a" pr_expression e
  | Pexp_poly (e, ct) -> pr_poly ppf e ct
  | Pexp_object c -> fprintf ppf "object@ %a@ end" pr_class_struct c
  | Pexp_open (modid, e) ->
    fprintf ppf "let open@ %a in@ %a"
      pr_ident_in_prefix_position modid pr_expression e
  | Pexp_pack (modexp, pkgty) ->
    fprintf ppf "%a :@ %a" pr_module_expr modexp pr_package_type pkgty
  | Pexp_newtype (t, e) -> fprintf ppf "(type@ %s)@ %a" t pr_expression e

and pr_let_in ppf rf pel e =
  let let_kwd = match rf with
    | Nonrecursive | Default -> "let"
    | Recursive -> "let rec" in
  let let_def_step kwd ppf (pat, exp) =
    fprintf ppf  "@[<hv 2>%s %a =@ @[<hv 0>%a@]@]" kwd
      pr_pattern pat pr_expression exp in
  match pel with
  | [] -> ()
  | pe :: pes ->
    fprintf ppf
      "@[<hv 0>\
       @[%a%a in@]\
       @ \
       @[<hv 0>%a@]\
       @]"
      (let_def_step let_kwd) pe
      (fun ppf ->
       List.iter (fun pe -> fprintf ppf "@ %a" (let_def_step "and") pe)) pes
      print_expression e

and pr_function ppf label eopt clauses =
  fprintf ppf
    "function@ \
     %s%a@,\
     %a"
    label
    (option pr_expression) eopt
    pr_clauses clauses

and pr_when_cond ppf = print_expression ppf
and pr_clause_right_side ppf = print_expression ppf

and pr_clause ppf (pat, e) =
  match pat, e.pexp_desc with
  | pat, Pexp_when (e1, e2) ->
    fprintf ppf
      "| @[<hv 0>\
         @[<hv 2>\
         %a@ \
           when@ \
           %a\
         @] ->@ \
         %a\
         @]"
      print_pattern pat
      pr_when_cond e1
      pr_clause_right_side e2

  | pat, _ ->
    fprintf ppf
      "| @[<hov 0>\
         %a ->@ \
         %a\
         @]"
      print_pattern pat
      pr_clause_right_side e

and pr_clauses ppf  clauses =
  match clauses with
  | [] -> assert false
  | cl :: cls ->
    fprintf ppf
      "@[<v 0>%a%a@]"
      pr_clause cl
      (fun ppf -> List.iter (fun cl -> fprintf ppf "@ %a" pr_clause cl)) cls

and pr_infix ppf e1 id e2 =
  pr_as_infix pr_expression expr_list_by_extension ppf e1 (Lident id) e2

and pr_apply ppf e args =
  let pr_lbl_expression ppf (lbl, e) =
    fprintf ppf "%a%a" pr_label lbl pr_expression e in
  let pr_lbl_expressions = pr_space_vals pr_lbl_expression in
  let pr_fun_caller = print_expression in
  let pr_fun_args = pr_lbl_expressions in
  pr_application
    pr_fun_caller pr_fun_args
    ppf e args

and pr_match ppf kwd e cs =
  fprintf ppf
    "@[<v 0>\
     @[<hov 0>%s@ %a@ with@]@ \
     %a\
     @]"
    kwd
    pr_expression e
    pr_clauses cs

and pr_tuple ppf es =
  fprintf ppf "(%a)" (pr_as_tuple pr_expression) es

and pr_record ppf fds with_opt = pr_as_record pr_expression ppf fds with_opt

and pr_setfield ppf r lbl e =
  fprintf ppf "@[%a.%a <-@ %a@]"
    pr_expression r pr_ident_in_prefix_position lbl pr_expression e

and pr_if ppf if_expr then_expr else_opt =
  let print_if_line kwd ppf e =
    fprintf ppf "%s @[%a@]" kwd pr_expression e in
  fprintf ppf "@[<hv 0>%a@ %a@ %a@]"
    (print_if_line "if") if_expr
    (print_if_line "then") then_expr
    (option (print_if_line "else")) else_opt

and pr_sequence ppf e1 e2 =
  fprintf ppf "@[<hv 0>begin@ %a;@ %a@ end@]"
    pr_expression e1 pr_expression e2

and pr_while ppf e1 e2 =
  fprintf ppf "@[<hv 0>@[while@ %a@ do@]@ %a@ done@]" pr_expression e1
    pr_expression e2

and pr_for ppf idx e1 e2 dir_flag e3 =
  let kwd = match dir_flag with
    | Upto -> "to"
    | Downto -> "downto" in
  fprintf ppf
    "@[<hv 0>\
     @[<hov 2>\
     for %a@ \
       = %a@ \
       %s %a\
     @]@ \
     @[<hov 2>\
     do@,\
       %a\
     @]@ \
     done@]"
    pr_ident_name_in_prefix_position idx
    pr_expression e1 kwd pr_expression e2 pr_expression e3

and pr_send ppf e s =
  fprintf ppf "%a#%s" pr_expression e s

and pr_setinsvar ppf s e = fprintf ppf "@[<hv 2>%s <-@ %a@]" s pr_expression e

and pr_override ppf l =
  fprintf ppf "{< @[<hov 0>%a@] >}" (pr_suffix_vals "; " pr_over_field) l

and pr_over_field ppf (s, e) = fprintf ppf "@[<hv 2>%s =@ %a@]" s pr_expression e

and pr_let_module_in ppf s m e =
  fprintf ppf
    "let module %s@ %a in@ %a"
    s
    pr_module_binding m
    pr_expression e

and pr_poly ppf e ctopt =
  match ctopt with
  | Some pty -> fprintf ppf ":@ %a =@ %a" pr_core_type pty pr_expression e
  | None -> fprintf ppf "%a" pr_expression e

(* Printing structure items *)
and pr_structure ppf sis =
  let structure_step ppf si =
    fprintf ppf
      "%a@ \
      ;;@ \
      @ "
      pr_structure_item si in

  fprintf ppf "\
    @[<v 0>\
    %a\
    @]"
    (fun ppf -> List.iter (structure_step ppf))
    sis

and pr_structure_item ppf str_item =
  pr_structure_item_desc ppf str_item.pstr_desc

and pr_exception_definition ppf name args =
  fprintf ppf "@[<hv 2>exception %s%a@]"
    name pr_constructor_type_arguments args

and pr_structure_item_desc ppf = function
  | Pstr_eval expr -> pr_expression ppf expr
  | Pstr_value (rf, pel) -> pr_value ppf rf pel
  | Pstr_primitive (name, val_descr) ->
      pr_primitive_definition ppf name val_descr.pval_type val_descr.pval_prim
  | Pstr_type s_ty_defs ->
      pr_type_definitions ppf s_ty_defs
  | Pstr_exception (name, args) ->
      pr_exception_definition ppf name args
  | Pstr_exn_rebind (name, lid) ->
      fprintf ppf "@[<hv 2>exception %s =@ %a@]"
        name pr_ident_in_prefix_position lid
  | Pstr_module (name, me) ->
      fprintf ppf "@[<hv 2>module %s = %a@]" name pr_module_expr me
  | Pstr_recmodule mod_bds -> pr_module_rec ppf mod_bds
  | Pstr_modtype (ty, modty) ->
      fprintf ppf "@[<hv 2>module type %s = %a@]" ty pr_module_type modty
  | Pstr_open lid ->
      fprintf ppf "open %a" pr_ident_in_prefix_position lid
  | Pstr_include me ->
      fprintf ppf "include@ %a" pr_module_expr me
  | Pstr_class _ ->
      not_yet_implemented "Printing of Pstr_class."
  | Pstr_class_type _ ->
      not_yet_implemented "Printing of Pstr_class_type."

and pr_module_rec ppf =
  let step kwd ppf (name, mt, me) =
    fprintf ppf  "@[%s %a:@ %a =@;<1 2>@[<2>%a@]@]"
      kwd pr_ident_name_in_prefix_position name
      pr_module_type mt pr_module_expr me in
  let steps kwd ppf vs =
    List.iter
     (fun v -> fprintf ppf "@ @ %a" (step kwd) v)
     vs in

  function
  | [] -> ()
  | v :: vs ->
    fprintf ppf "@[<v 0>%a%a@]"
      (step "module rec") v
      (steps "and") vs

and pr_value ppf rec_flag =
  let let_kwd = match rec_flag with
    | Asttypes.Nonrecursive | Asttypes.Default -> "let"
    | Asttypes.Recursive -> "let rec" in
  let value_step kwd ppf (pat, e) =
    fprintf ppf  "@[%s %a =@;<1 2>@[<2>%a@]@]" kwd
      pr_pattern pat pr_expression e in
  let value_steps kwd ppf vs =
    List.iter (fun v -> fprintf ppf "@ @ %a" (value_step kwd) v) vs in

  function
  | [] -> ()
  | v :: vs ->
    fprintf ppf "@[<v 0>%a%a@]"
      (value_step let_kwd) v
      (value_steps "and") vs

and pr_module_decl ppf mdecl =
  match mdecl.pmty_desc with
  | Pmty_functor (s, mty, md) ->
      fprintf ppf "@[(%s :@ %a)@ %a@]" s
        pr_module_type mty pr_module_decl md
  | _ -> fprintf ppf "@[:@ %a@]" pr_module_type mdecl

and pr_recmodule ppf (id, mty) =
  fprintf ppf "@[%s :@ %a@]" id pr_module_type mty

and pr_recmodules ppf rems =
  fprintf ppf "@[<v 2>%a@]" (pr_sep_vals_fmt "@;and " pr_recmodule) rems

and pr_signature ppf sigs =
  fprintf ppf "@[<v>%a@]" (pr_suffix_vals_fmt ";;@;" pr_signature_item) sigs

and pr_signature_item ppf sigitem =
  match sigitem.psig_desc with
    | Psig_value (name, { pval_type = ty; pval_prim = defs;}) ->
    (match defs with
     | [] ->
       fprintf ppf "@[val@ %a @: %a@]"
         pr_ident_name_in_prefix_position name pr_core_type ty
     | _ ->
       pr_primitive_definition ppf name ty defs
    )
  | Psig_type tdl ->
    pr_type_definitions ppf tdl
  | Psig_exception (name, args) ->
    pr_exception_definition ppf name args
  | Psig_module (name, mdecl) ->
    fprintf ppf "@[module@ %s@ %a@]" name pr_module_decl mdecl
  | Psig_recmodule mrdecls ->
    fprintf ppf "@[module rec@ %a@]" pr_recmodules mrdecls
  | Psig_modtype (name, modty) ->
    (match modty with
     | Pmodtype_abstract ->
       fprintf ppf "@[module type@ %s@]" name
     | Pmodtype_manifest mty ->
       fprintf ppf "@[<v 2>module type %s =@;@[%a@]@]"
         name pr_module_type mty)
  | Psig_open id ->
    fprintf ppf "@[open@ %a@]" pr_ident_in_prefix_position id
  | Psig_include mty ->
    fprintf ppf "@[include@ %a@]" pr_module_type mty
  (* TODO : some more object-related things to print *)
  | Psig_class _cldescr ->
    not_yet_implemented "Printing of Psig_class."
  | Psig_class_type _cltdecl ->
    not_yet_implemented "Printing of Psig_class_type."
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
