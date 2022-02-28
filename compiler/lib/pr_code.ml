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

(* $Id: pr_code.ml,v 1.32 2012-06-04 13:01:22 weis Exp $ *)

open Format
open Code
;;

(* Borrowed from Caml AST pretty-printer. *)
let pr_ident_in_prefix_position = Pr_ocaml.pr_ident_in_prefix_position
and pr_ident_in_infix_position = Pr_ocaml.pr_ident_in_infix_position
and is_infix_longident = Pr_ocaml.is_infix_longident
;;

(* Functions to print #line directives.
   Setter functions are are called from driver/main.ml.
*)
let reset_line_count, incr_line_count, get_line_count =
  let to_line = ref 0 in
  (fun () -> to_line := 0),
  (fun () -> incr to_line),
  (fun () -> !to_line)
;;

let set_output_filename, get_output_filename =
  let fname = ref "" in
  (fun s -> fname := s),
  (fun () -> !fname)
;;


(* Printing the code. *)

type printing_context =
   | Ctx_top
   | Ctx_fun
   | Ctx_fun_arg
   | Ctx_infix_arg
   | Ctx_if_guard
   | Ctx_let_def
   | Ctx_let_in
   | Ctx_match
   | Ctx_clause_when
   | Ctx_clause_right
   | Ctx_pattern
;;

(* Parentheses *)

let parens fmt = "(" ^^ fmt ^^ ")"
;;

let parens_match fmt = function
  | Ctx_top -> fmt
  | Ctx_fun -> fmt
  | Ctx_fun_arg -> parens fmt
  | Ctx_infix_arg -> fmt
  | Ctx_if_guard -> fmt
  | Ctx_let_def -> fmt
  | Ctx_let_in -> fmt
  | Ctx_match -> fmt
  | Ctx_clause_when -> parens fmt
  | Ctx_clause_right -> parens fmt
  | Ctx_pattern -> parens fmt
;;

let parens_apply fmt = function
  | Ctx_top -> fmt
  | Ctx_fun -> parens fmt
  | Ctx_fun_arg -> parens fmt
  | Ctx_infix_arg -> fmt
  | Ctx_if_guard -> fmt
  | Ctx_let_def -> fmt
  | Ctx_let_in -> fmt
  | Ctx_match -> fmt
  | Ctx_clause_when -> fmt
  | Ctx_clause_right -> fmt
  | Ctx_pattern -> fmt
;;

let parens_infix fmt = function
  | Ctx_top -> fmt
  | Ctx_fun -> parens fmt
  | Ctx_fun_arg -> parens fmt
  | Ctx_infix_arg -> parens fmt
  | Ctx_if_guard -> fmt
  | Ctx_let_def -> fmt
  | Ctx_let_in -> fmt
  | Ctx_match -> fmt
  | Ctx_clause_when -> fmt
  | Ctx_clause_right -> fmt
  | Ctx_pattern -> parens fmt
;;

let parens_if fmt = function
  | Ctx_top -> fmt
  | Ctx_fun -> parens fmt
  | Ctx_fun_arg -> parens fmt
  | Ctx_infix_arg -> parens fmt
  | Ctx_if_guard -> fmt
  | Ctx_let_def -> fmt
  | Ctx_let_in -> fmt
  | Ctx_match -> parens fmt
  | Ctx_clause_when -> parens fmt
  | Ctx_clause_right -> fmt
  | Ctx_pattern -> parens fmt
;;

let parens_let fmt = function
  | Ctx_top -> fmt
  | Ctx_fun -> parens fmt
  | Ctx_fun_arg -> parens fmt
  | Ctx_infix_arg -> parens fmt
  | Ctx_if_guard -> fmt
  | Ctx_let_def -> fmt
  | Ctx_let_in -> fmt
  | Ctx_match -> parens fmt
  | Ctx_clause_when -> parens fmt
  | Ctx_clause_right -> parens fmt
  | Ctx_pattern -> parens fmt
;;

let parens_sequence fmt = function
  | Ctx_top -> fmt
  | Ctx_fun -> parens fmt
  | Ctx_fun_arg -> parens fmt
  | Ctx_infix_arg -> parens fmt
  | Ctx_if_guard -> parens fmt
  | Ctx_let_def -> parens fmt
  | Ctx_let_in -> fmt
  | Ctx_match -> parens fmt
  | Ctx_clause_when -> parens fmt
  | Ctx_clause_right -> fmt
  | Ctx_pattern -> fmt
;;

(*
let parens_tuple fmt = function
  | Ctx_top -> fmt
  | Ctx_fun -> fmt
  | Ctx_fun_arg -> parens fmt
  | Ctx_infix_arg -> parens fmt
  | Ctx_if_guard -> parens fmt
  | Ctx_let_def -> fmt
  | Ctx_let_in -> fmt
  | Ctx_match -> fmt
  | Ctx_clause_when -> parens fmt
  | Ctx_clause_right -> parens fmt
  | Ctx_pattern -> parens fmt
;;
*)

(* Printing special syntax for lists by extension *)
let ident_list_nil = Longident.Lident "[]"
and ident_list_cons = Longident.Lident "::"
;;

let is_cons_ident lid = lid = ident_list_cons
and is_nil_ident lid = lid = ident_list_nil
;;

let list_by_extension =

  let rec accumulate_list_by_extension accu = function
  | Constant lid when is_nil_ident lid -> Some (List.rev accu)
  | Generator (gid, _) when is_nil_ident gid -> Some (List.rev accu)
  | Infix (e1, gid, e2) when is_cons_ident gid ->
    accumulate_list_by_extension (e1 :: accu) e2

  | Constant _ | Var _ | Ocaml_pattern _ | Ocaml_expression _
  | Match _  | Try _ | Generator _ | Apply _ | Infix _ | If _
  | Let _ | Letrec _ | Sequence _ | Record _ | Tuple _ | List _
  | Function _ | Sline_directive _ | Comment (_, _) -> None in

  (fun e -> accumulate_list_by_extension [] e)
;;

(* Printing expressions *)

(* Prints a prefix, then a space and an item,
   and iterates that on a list of items. *)
let pr_prefix_exps prefix pr ppf =
  let pr_item item = fprintf ppf "%s@ %a" prefix pr item in
  List.iter pr_item
;;

let pr_sep_exps ctx sep pr ppf = function
  | [] -> ()
  | e :: es ->
    fprintf ppf "@[<hov 0>%a@]" (pr ctx) e;
    pr_prefix_exps sep (pr ctx) ppf es
;;

let pr_space_exps pr ppf = pr_prefix_exps "" pr ppf
;;

let rec pr_exp ctx ppf = function
  | Constant lid -> fprintf ppf "%a" pr_ident_in_prefix_position lid
  | Var lid -> fprintf ppf "%a" pr_ident_in_prefix_position lid
  | Ocaml_pattern p -> pr_ocaml_pattern Ctx_top ppf p
  | Ocaml_expression e -> pr_ocaml_expression ctx ppf e
  | Match (e, cs) -> pr_match ctx ppf "match" e cs
  | Try (e, cs) -> pr_match ctx ppf "try" e cs
  | Generator (gid, e) when is_infix_longident gid ->
    pr_infix_generator ctx ppf gid e
  | Generator (gid, e) -> pr_generator ctx ppf gid e
  | Apply (e, l) -> pr_apply ctx ppf e l
  | Infix (e1, lid, e2) -> pr_infix ctx ppf e1 lid e2
  | If (e1, e2, e3) -> pr_if ctx ppf e1 e2 e3
  | Let (name, args, e1, e2) -> pr_let_in "let" ctx ppf name args e1 e2
  | Letrec (name, args, e1, e2) -> pr_let_in "let rec" ctx ppf name args e1 e2
  | Sequence (e1, e2, es) -> pr_sequence ctx ppf e1 e2 es
  | Record fs -> pr_record ctx ppf fs
  | Tuple es -> pr_tuple ctx ppf es
  | List es -> pr_list ctx ppf es
  | Function cs -> pr_function ctx ppf cs
(*  | Comment (s, e) -> fprintf ppf "@[<hv 0>(* %s *)@\n%a@]" s (pr_exp ctx) e *)
  | Sline_directive (n, s, e) -> pr_sline ctx ppf n s e
  | Comment (c, e) ->
    fprintf ppf "@[<v>(* %s *)@,%a@]" c (pr_exp ctx) e

and pr_sline c ppf n s e =
  if n = 0 || s = ""
  then
    begin
      fprintf ppf "@[<v>(* Default generated clause *)@,%a@]" (pr_default c) e;
    end
  else
    begin
      let lc = get_line_count () in
      fprintf ppf "@[<v>\
                     @[<v>\n# %d \"%s\"@]@,\
                    %a@[<v>\n# %d \"%s\"@]@]"
        n s (pr_default c) e lc (get_output_filename ());
    end

and pr_default c ppf e = pr_exp c ppf e

and pr_ocaml_expression ctx ppf p =
  let fmt = parens_sequence "%a" ctx in
  fprintf ppf fmt Pr_ocaml.pr_expression p

and pr_ocaml_pattern ctx ppf p =
  let fmt = parens_sequence "%a" ctx in
  fprintf ppf fmt Pr_ocaml.pr_pattern p

and pr_match ctx ppf kwd e cs =
  let fmt_string =
    parens_match
      "@[<v 0>\
       @[<hov 0>%s@ %a@ with@]@ \
       %a\
       @]"
      ctx in
  fprintf ppf fmt_string kwd pr_match_exp e pr_clauses cs

and pr_match_exp ppf e = pr_exp Ctx_match ppf e

and pr_clause ppf = function
  | Clause_when (p, w, e) ->
    fprintf ppf
      "| @[<hv 0>\
         @[<hv 2>\
         %a@ \
           when@ \
           %a\
         @] ->@ \
         %a\
         @]"
      pr_pattern p
      pr_when_cond w
      pr_clause_right_side e

  | Clause (p, e) ->
    fprintf ppf
      "| @[<hov 0>\
         %a ->@ \
         %a\
         @]"
      pr_pattern p
      pr_clause_right_side e

and pr_when_cond ppf e = pr_exp Ctx_clause_when ppf e

and pr_clause_right_side ppf e = pr_exp Ctx_clause_right ppf e

and pr_clauses ppf clauses =
  match clauses with
  | [] -> assert false
  | cl :: cls ->
    fprintf ppf
      "@[<v 0>%a%a@]"
      pr_clause cl
      (fun ppf -> List.iter (fun cl -> fprintf ppf "@ %a" pr_clause cl)) cls

and pr_infix_generator ctx ppf gid e =
  match e with
  | Tuple [ e1; e2; ] -> pr_infix ctx ppf e1 gid e2
  | Constant _ | Var _ | Ocaml_pattern _
  | Ocaml_expression _ | Match _ | Try _
  | Generator _ | Apply _ | Infix _ | If _ | Let _
  | Letrec _ | Sequence _ | Record _ | Tuple _ | List _
  | Function _
  | Sline_directive _
  | Comment (_, _) -> pr_generator ctx ppf gid e

and pr_generator ctx ppf gid e =
  let fmt_string = parens_apply "@[<hv 2>%a%a@]" ctx in
  fprintf ppf fmt_string
    pr_ident_in_prefix_position gid
    pr_fun_args [e]

and pr_apply ctx ppf e l =
  let fmt_string =
    parens_apply
      "@[<hv 2>\
       @[<hov 0>%a@]%a\
       @]" ctx in
  fprintf ppf fmt_string
    pr_fun_caller e
    pr_fun_args l

and pr_fun_caller ppf e = pr_exp Ctx_fun ppf e

and pr_fun_args ppf args = pr_space_exps (pr_exp Ctx_fun_arg) ppf args

and pr_infix ctx ppf e1 lid e2 =
  if is_cons_ident lid
  then pr_list_cons ctx ppf e1 e2
  else pr_infix_operator ctx ppf e1 lid e2

and pr_infix_operator ctx ppf e1 lid e2 =
  let fmt_string = parens_infix "@[<hov 0>%a@ %a@ %a@]" ctx in
  fprintf ppf fmt_string pr_infix_arg
    e1 pr_ident_in_infix_position lid pr_infix_arg e2

and pr_infix_arg ppf e = pr_exp Ctx_infix_arg ppf e

and pr_list_cons ctx ppf e1 e2 =
  match list_by_extension e2 with
    | None -> pr_infix_operator ctx ppf e1 ident_list_cons e2
    | Some es -> pr_list ctx ppf (e1 :: es)

and pr_if ctx ppf e1 e2 e3 =
  let fmt_string =
    parens_if "@[<hv 0>if @[<hov 0>%a@]@ then @[<hov 0>%a@]@ else @[<hov 0>%a@]@]" ctx in
  fprintf ppf fmt_string pr_if_guard e1 pr_if_branch e2 pr_if_branch e3

and pr_if_guard ppf e = pr_exp Ctx_if_guard ppf e

and pr_if_branch ppf e = pr_exp Ctx_let_in ppf e

and pr_let_in kwd ctx ppf name args e1 e2 =
  let fmt_string =
    parens_let
      "@[<hv 0>\
       @[<hv 2>\
       %s @[<hv 2>%a%a@] =@ \
         %a\
       @] in@ \
       %a\
       @]"
      ctx in
  fprintf ppf fmt_string
    kwd pr_ident_in_prefix_position name pr_patterns args
    pr_let_def_expr e1
    pr_let_in_expr e2

and pr_let_def_expr ppf e = pr_exp Ctx_let_def ppf e

and pr_let_in_expr ppf e = pr_exp Ctx_let_in ppf e

and pr_pattern ppf p = pr_exp Ctx_pattern ppf p

and pr_patterns ppf ps  = pr_space_exps pr_pattern ppf ps

and pr_sequence ctx ppf e1 e2 es =
  let fmt_string = parens_sequence "@[<v 0>%a@]" ctx in
  fprintf ppf fmt_string (pr_sep_exps ctx ";" pr_list_elem) (e1 :: e2 :: es)

and pr_record ctx ppf fds =
  fprintf ppf "{ @[<hov 0>%a@] }" (pr_fields ctx) fds

and pr_fields ctx ppf fds = pr_sep_exps ctx ";" pr_field ppf fds

and pr_field ctx ppf (name, e) =
  fprintf ppf "%a =@ %a" pr_ident_in_prefix_position name (pr_list_elem ctx) e

(* Why using parens systematically ? *)
and pr_tuple ctx ppf es =
  fprintf ppf
    "(@[<hov 0>%a@])"
    (pr_sep_exps ctx "," pr_list_elem) es

and pr_list ctx ppf es =
  fprintf ppf "[ @[<hov 0>%a@] ]" (pr_sep_exps ctx ";" pr_list_elem) es

and pr_list_elem ctx ppf e = pr_exp ctx ppf e

and pr_function ctx ppf cs =
  let fmt_string =
    parens_match
      "function@\n%a"
      ctx in
  fprintf ppf fmt_string pr_clauses cs
;;

(* Printing structure items *)

let pr_value ppf rec_flag = function
  | [] -> ()
  | v :: vs ->

    let value_step kwd ppf (ident, args, e) =
      fprintf ppf
        "@[<hv 2>\
         %s @[<hv 2>%a%a@] =@ \
           %a\
         @]"
        kwd
        pr_ident_in_prefix_position ident
        pr_patterns args
        pr_let_def_expr e in

    let value_step_and ppf v =
      fprintf ppf
        "@ \
         @ \
         %a"
        (value_step "and")
        v in

    let value_steps ppf vs =
      List.iter (value_step_and ppf) vs in

    let let_kwd = match rec_flag with
      | Asttypes.Nonrecursive | Asttypes.Default -> "let"
      | Asttypes.Recursive -> "let rec" in

    fprintf ppf
      "@[<v 0>\
       %a%a\
       @]"
      (value_step let_kwd) v
      value_steps vs
;;

let pr_expression ppf expr =
  pr_exp Ctx_top ppf expr
;;

let rec pr_structure_item ppf = function
  | Pstr_eval expr -> pr_expression ppf expr
  | Pstr_value (rf, pel) -> pr_value ppf rf pel
  | Pstr_primitive (str, val_descr) ->
    Pr_ocaml.pr_structure_item_desc ppf (Parsetree.Pstr_primitive (str, val_descr))
  | Pstr_type s_ty_decls ->
    Pr_ocaml.pr_structure_item_desc ppf (Parsetree.Pstr_type s_ty_decls)
  | Pstr_exception (str, args) ->
    Pr_ocaml.pr_structure_item_desc ppf (Parsetree.Pstr_exception (str, args))
  | Pstr_open module_lid ->
    Pr_ocaml.pr_structure_item_desc ppf (Parsetree.Pstr_open module_lid)
  | Pstr_include me ->
    Pr_ocaml.pr_structure_item_desc ppf (Parsetree.Pstr_include me)
  | Pstr_comment (cmt, sitem) ->
    Format.fprintf ppf
      "@[<v 0>\
       (* %s *)@ \
       @ %a\
       @]"
      cmt
      pr_structure_item sitem
;;

let pr_structure ppf sis =
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
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
