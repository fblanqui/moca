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

(* $Id: genr_sharing.ml,v 1.81 2012-04-02 17:46:53 weis Exp $ *)

open Parsetree
open Longident
open Code
open Genr_base
open Otype
open Check
;;

(* The modules we generate. *)
let hashing_module_ident = prefix_longident "Hashing_"
and sharing_module_ident = prefix_longident "Sharing_"
;;

(* The name of the private module that defines the relational type. *)
let type_private_module_ident = prefix_longident "Type_"
;;

(* The name of the type as defined in its private module. *)
let type_ident_in_private_module tid =
  let tname = name_of_longident tid in
  Ldot (type_private_module_ident tid, tname)
;;

let type_obj_ident = Ldot (Lident "Obj", "t")
;;

(* The functions we generate. *)
let equal_function_ident = prefix_longident "equal_"
and hash_function_ident = prefix_longident "hash_"
and get_hash_function_ident tid = Code.prefix_longident "get_hash_" tid
;;

(* The name of the sharing hash table. *)
let sharing_hash_table_ident = prefix_longident "htbl_"
;;
let initial_sharing_hash_table_size = 1009
;;

(* The name of the system hash function. *)
let default_hash_function_ident = Ldot (Lident "Hashtbl", "hash")
;;

(* Which function to use to hash arguments of generator gname ? *)
let get_hash_function_ident_for_arg_i tid gname i =
  (* The default hash function is the one of the [Hashtbl] module of the
     standard library. *)
  try
    let gi = Check.find_generator_name_info gname in
    let gt = List.nth gi.gi_arg_types (i - 1) in
    begin match gt.ptyp_desc with
     | Ptyp_constr ((Lident _ as gtid), _) when gtid = tid ->
       get_hash_function_ident tid
     | _ ->
       default_hash_function_ident end with
  | Not_found -> default_hash_function_ident
;;

(* Which function to use to compare the arguments of the generator gname ? *)
let infix_eq_function eq x y = infix_apply x (Lident eq) y;;
let eq_function = infix_eq_function "==";;
let equal_function = infix_eq_function "=";;

let rec get_eq_function_name_for_type tname typ =
  match typ.ptyp_desc with
  (* Comparison between values of a maximally shared type: use ==. *)
  | Ptyp_constr (Lident s, _) when s = tname -> eq_function
  (* Constant type constructors *)
  | Ptyp_constr (Lident s, []) ->
    begin match s with
    | "int" | "char" | "bool" | "unit" -> eq_function
    | "string" | "float" | "exn"
    | "nativeint" | "int32" | "int64" -> equal_function
    (* Other constant type constructors. *)
    (*| Ptyp_constr (id, []) ->*)
    (* Conservative approach:
       in any other case use the generic = function. *)
    | _ -> equal_function end
  (* Functional type constructors *)
    (* Conservative approach:
       in any other case use the generic = function. *)
  | _ -> equal_function
;;

let get_eq_fun_for_arg_i tname gname i =
  try
    let gi = Check.find_generator_name_info gname in
    let arg_i_typ = List.nth gi.gi_arg_types (i - 1) in
    begin match arg_i_typ.ptyp_desc with
     (* Comparison between values of a maximally shared type: use ==. *)
     | Ptyp_constr (Lident s, _) when s = tname -> infix_eq_function "=="
     | _ -> get_eq_function_name_for_type tname arg_i_typ
    end with
  | Not_found -> infix_eq_function "="
;;

let get_eq_function_for_arg_i tid gname i =
  let tname = name_of_longident tid in
  get_eq_fun_for_arg_i tname gname i
;;

(* The type info that would contain the hash key for a value of
   the relational type. *)
let typ_info = Otype.constant_type "info"
;;

let add_info_to_generator_type ((gid, tycl, rels, loc) as cstr) =
  match tycl with
  | [] -> cstr
  | _ -> (gid, typ_info :: tycl, rels, loc)
;;

let add_info_arg_to_type_kind = function
  | Ptype_abstract _relations as x -> x
  | Ptype_variant cdefs ->
      Ptype_variant (List.map add_info_to_generator_type cdefs)
  | Ptype_record _flds as x -> x
;;

let add_info_arg_to_type_declaration td =
  { td with ptype_kind = add_info_arg_to_type_kind td.ptype_kind }
;;

let find_typedef_params tid =
  let tname = name_of_longident tid in
  let type_decl = find_type_name_declaration tname in
  type_decl.ptype_params
;;

let is_typedef_polymorphic tid =
  find_typedef_params tid != []
;;

let print_type_params = Pr_ocaml.pr_params
;;

let longident = Pr_ocaml.pr_ident_in_prefix_position
;;

let print_type_shared ppf tid =
  let ty_vars = find_typedef_params tid in
  Format.fprintf ppf "@[%a%a@]"
    print_type_params ty_vars
    longident (type_ident_in_private_module tid)
;;

let print_hashing_module ppf tid =
  Format.pp_open_vbox ppf 0;
  if not (is_typedef_polymorphic tid) then begin
    (* The monomorphic case. *)
    (* We output the module that defines the hash function. *)
    Format.fprintf ppf "\
      @[<v 2>@[module@ %a =@ struct@]@;\
      @[type t =@ %a;;@]@;\
      @[let equal =@ %a;;@]@;\
      @[let hash =@ %a;;@]@]"
      longident (hashing_module_ident tid)
      longident (type_ident_in_private_module tid)
      longident (equal_function_ident tid)
      longident (hash_function_ident tid);
    Format.fprintf ppf "@;end@;;;@;@;";
    (* We output the module that defines the sharing hash table. *)
    Format.fprintf ppf "\
      @[module@ %a =@ Weak.Make (%a);;@]"
      longident (sharing_module_ident tid)
      longident (hashing_module_ident tid);
    Format.fprintf ppf "@;@;";
  end else begin
    (* The polymorphic case. *)
    let print_equal_function_body ppf fid =
      Format.fprintf ppf
        "@[<1>fun x y ->@ %a@ (Obj.magic x)@ (Obj.magic y)@]"
        longident fid in
    let print_hash_function_body ppf fid =
      Format.fprintf ppf
        "@[fun x ->@ %a@ (Obj.magic x :@ %a)@]"
        longident fid
        print_type_shared tid in
    (* We output the module that defines the hash function. *)
    Format.fprintf ppf "\
      @[<v 2>@[module@ %a =@ struct@]@;\
      @[type t =@ %a;;@]@;\
      @[let equal =@ %a;;@]@;\
      @[let hash =@ %a;;@]@]"
      longident (hashing_module_ident tid)
      longident type_obj_ident
      print_equal_function_body (equal_function_ident tid)
      print_hash_function_body (hash_function_ident tid);
    Format.fprintf ppf "@;end@;;;@;@;";
    (* We output the module that defines the sharing hash table. *)
    Format.fprintf ppf "\
      @[<v 2>@[module@ %a =@ struct@]@;\
      @[module Share =@ Weak.Make (%a);;@]@;\
      @[include Share;;@]@;\
      @[<1>let find t x =@ (Obj.magic@ (Share.find t@ (Obj.repr x)));;@]@;\
      @[let add t x =@ Share.add t@ (Obj.repr x);;@]@]"
      longident (sharing_module_ident tid)
      longident (hashing_module_ident tid);
    Format.fprintf ppf "@;end@;;;@;@;";
  end;
  (* We define the sharing hash table. *)
  Format.fprintf ppf "\
    @[let %a =@ %a.create@ %i@]"
    longident (sharing_hash_table_ident tid)
    longident (sharing_module_ident tid)
    initial_sharing_hash_table_size;
  Format.fprintf ppf "@;;;@;";
  Format.pp_close_box ppf ();
;;

let generator_info gname info args =
  Generator (Lident gname, Tuple (info :: args))
;;

let genr_clause_nullary i gname =
  let genr_constant_pattern gname = Code.make_Constant gname in
  Clause (genr_constant_pattern gname, make_Constant (string_of_int i))
;;

let genr_clauses clause_nullary clause_non_nullary cdefs default_clauses =
  (* i is the rank of the current nullary generator.
     j is the rank of the current non-nullary generator. *)
  let rec loop (i, j, accu) = function
    | [] -> accu
    | (_gname, gts, _rels, _loc as cdef) :: cdefs ->
      match List.length gts with
      | 0 ->
        let accu = clause_nullary i cdef accu in
        loop (i + 1, j, accu) cdefs
      | n ->
        let accu = clause_non_nullary j cdef n accu in
        loop (i, j + 1, accu) cdefs in
  loop (1, 1, default_clauses) cdefs
;;

(* Generates the hash table internal equality function for (sum) type tname
   (the function named equal_tname in the generated source). *)
let genr_equal_sum tid cdefs =

  let args = [x; y] in

  let clause_nullary _i _cdef accu = accu

  and clause_non_nullary _j cdef n accu =
    let xs = genr_args "x" n
    and ys = genr_args "y" n in

    let g = Otype.generator_of_cdef cdef in

    let p1 = Genr_base.pattern_info g underscore xs
    and p2 = Genr_base.pattern_info g underscore ys in
    let i_xs = genr_numbered_args "x" n in
    let gname =
      match g.Parsetree.pgen_desc with
      | Lident name -> name
      | Lapply (_, _)
      | Ldot (_, _) -> assert false in
    let eqs =
      List.map2
        (fun (i, x) y -> get_eq_function_for_arg_i tid gname i x y)
        i_xs ys in
    let body =
      match eqs with
      | eq :: eqs ->
        List.fold_left (fun eq e -> infix_name eq "&&" e) eq eqs
      | _ -> assert false in
    Clause (pair p1 p2, body) :: accu

  and default_clauses = [Clause (underscore, make_Constant "false")] in

  let clauses =
    genr_clauses clause_nullary clause_non_nullary cdefs default_clauses in

  topletrec [equal_function_ident tid, args, Match (pair x y, clauses)]
;;

let info_pattern h = Record [Lident "hash", make_Constant h]
;;

(* Finds the hash code in the tname values, retrieved in the info fiels. *)
let genr_get_hash_sum tid cdefs =

  let args = [x] in

  let clause_nullary i (gname, _, _, _) accu = genr_clause_nullary i gname :: accu

  and clause_non_nullary _j cdef n accu =

    let g = Otype.generator_of_cdef cdef in

    Clause
     (pattern_info g (info_pattern "h") (Code.genr_underscores n),
      make_Constant "h") ::
    accu

  and default_clauses = [] in

  let clauses =
    genr_clauses clause_nullary clause_non_nullary cdefs default_clauses in

  topletrec [get_hash_function_ident tid, args, Match (x, clauses)]
;;

(* Generates the hashing function for (sum) type tname
   (the function named hash_tname in the generated code). *)
let genr_hash_sum tid cdefs =

  let args = [x] in

  let clause_nullary i (gname, _, _, _) accu = genr_clause_nullary i gname :: accu

  and clause_non_nullary j (gname, _, _, _ as cdef) n accu =

    let g = Otype.generator_of_cdef cdef in

    (* The arguments of generator gname. *)
    let i_xis = genr_numbered_args "x" n in
    (* The integer value associated to the generator is its unique rank
       in the type definition.
       This value is added to the sum of all the hash codes of the generator
       arguments. *)
    let ghash = make_Constant (string_of_int j) in
    let sum e (i, xi) =
      infix_name
        (apply1 (get_hash_function_ident_for_arg_i tid gname i) xi) "+" e in
    let clause_exp = List.fold_left sum ghash i_xis in
    Clause (pattern_info g Code.underscore (Code.genr_args "x" n), clause_exp) :: accu

  and default_clauses = [] in

  let clauses =
    genr_clauses clause_nullary clause_non_nullary cdefs default_clauses in

  topletrec [hash_function_ident tid, args, Match (x, clauses)]
;;

let genr_construction_function tid cdef = function
  | 0 -> assert false
  | n ->

    let g = generator_of_cdef cdef in
    let gname = name_of_generator g in

    let args = Code.genr_args "x" n
    and info = make_Constant "info"
    and v = make_Constant "v"
    and hinfo = make_Constant "info.hash" in
    let sharing_module_ident = sharing_module_ident tid
    and tbl = Constant (sharing_hash_table_ident tid) in

    let find_ident = Ldot (sharing_module_ident, "find")
    and add_ident =  Ldot (sharing_module_ident, "add") in

    let with_clause =
      Clause
        (make_Constant "Not_found",
         Sequence (apply2 add_ident tbl v, v, [])) in

    let body =
      Let (Lident "info", [], Record [Lident "hash", make_Constant "0"],
      Let (Lident "v", [], generator_info gname info args,
      Sequence
        (infix_name hinfo "<-" (apply1 (hash_function_ident tid) v),
         Try (apply2 find_ident tbl v, [with_clause]),
         []))) in

    topletrec [mk_sharing_function_ident g, args, body]
;;

let genr_construction_functions tid cdefs =
  List.map
    (fun (_gname, gts, _rels, _loc as cdef) ->
      genr_construction_function tid cdef (List.length gts))
    (List.filter (fun (_gname, gts, _rels, _loc) -> List.length gts > 0) cdefs)
;;

let print_sharing_mli_preamble ppf =
  Format.fprintf ppf "@[<1>type info = private { mutable hash : int };;@]@;@;";
;;

let print_sharing_ml_preamble ppf =
  Format.fprintf ppf "@[type info = { mutable hash : int };;@]@;@;";
;;

let print_type_private_structure_items ppf ntds =
  let print_type_structure_items ppf (tname, td) =
    let structure_items = get_structure_items_of_type_declaration td in
    if structure_items <> [] then begin
      Format.pp_open_vbox ppf 0;
      Format.fprintf ppf "(* @[<1>Value defined from type %s@] *)@;" tname;
      Pr_ocaml.pr_verbatim_structure_items ppf structure_items;
      Format.pp_close_box ppf ();
    end in
  List.iter (print_type_structure_items ppf) ntds;
;;

let print_type_private_module ppf ntds =
  let private_module_ident =
    match ntds with
    | (tname, _td) :: _ntds -> type_private_module_ident (Lident tname)
    | _ -> assert false in
  let public_ntds =
    List.map
      (fun (tname, td) -> tname,
         add_info_arg_to_type_declaration
           (comment_relations_in_type_declaration
              (make_public_type_declaration td)))
    ntds in
  Format.fprintf ppf
    "@[<v 0>\
     @[<v 2>\
     module %a = struct@ \
       @[<v>\
       %a\
       %a\
       @]@ \
       @]@ \
     end@ \
;;@ \
     include %a@ \
;;@ \
     @]"
    longident private_module_ident
    Pr_ocaml.pr_type_definitions public_ntds
    print_type_private_structure_items ntds
    longident private_module_ident
;;

let is_recursive_sum_type tid cdefs =
  let rec is_tid ct =
    match ct.Parsetree.ptyp_desc with
    | Ptyp_any -> false
    | Ptyp_var _ -> false
    | Ptyp_arrow (_, _, _) -> false
    | Ptyp_constr (id, _) -> id = tid
    | Ptyp_object _ -> assert false
    | Ptyp_class (_, _, _) -> assert false
    | Ptyp_alias (_, _) -> assert false
    | Ptyp_variant (_, _, _) -> assert false
    | Ptyp_poly (_, _) -> assert false
    | Ptyp_package _ -> assert false
    | Ptyp_tuple cts -> List.exists is_tid cts
    | Ptyp_parens ct -> is_tid ct in

  List.exists
    (fun (_gname, gts, _rels, _loc) ->
     List.exists is_tid gts)
    cdefs
;;

let print_ntd ppf (tname, td) =
  match td.ptype_kind with
  | Ptype_abstract _rels ->
    begin match td.ptype_private with
    | Asttypes.Public -> assert false
    | Asttypes.Private -> assert false
    end
  | Ptype_variant cdefs ->
    let tid = Lident tname in
    let teq = genr_equal_sum tid cdefs in
    let tgh = genr_get_hash_sum tid cdefs in
    let teh = genr_hash_sum tid cdefs in
    (* If no generator of the type has an element of the type as argument then
       the function get_hash_t is useless (we would use Hashtbl.hash directely. *)
    if is_recursive_sum_type tid cdefs
    then Pr_code.pr_structure ppf [teq; tgh; teh]
    else Pr_code.pr_structure ppf [teq; teh];
    print_hashing_module ppf tid;
(*    if Genr_memo.get_memoize_target () then begin
      print_memo_hash_table_module ppf;
(* Pr_code.pr_structure ppf (genr_construction_function_memo_tables tid cdefs) *)
      end; *)
    Pr_code.pr_structure ppf (genr_construction_functions tid cdefs)
  | Ptype_record _fds -> assert false
;;

let print_sharing ppf ntds =
  print_type_private_module ppf ntds;
  List.iter (print_ntd ppf) ntds
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
