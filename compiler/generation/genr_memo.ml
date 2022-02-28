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

(* $Id: genr_memo.ml,v 1.3 2011-05-16 15:36:59 weis Exp $ *)

open Code
open Genr_base
open Longident
;;

(* Do we memoize generation functions ? *)
let set_memoize_target, get_memoize_target =
  let memoize_target = ref false in
  (fun () -> memoize_target := true),
  (fun () -> !memoize_target)
;;

let set_memo_table_size, get_memo_table_size =
  let memo_table_size = ref 1024 in
  (fun sz -> memo_table_size := sz),
  (fun () -> !memo_table_size)
;;

let mk_tuple_string_with_prefix prefix length =
  let rec construct_string n acc_s =
    if n >= length then acc_s else
    if n = 0
    then construct_string (n + 1) (Format.sprintf "%s%d" prefix n)
    else construct_string (n + 1) (Format.sprintf "%s, %s%d" acc_s prefix n)
  in Format.sprintf "(%s)" (construct_string 0 "")
;;

let print_memo_hash_table_module ppf =
  Format.fprintf ppf "\
    @[<v>\
    @[<v>\
    @[<v 2>\
    module Memo_htbl = struct@ \
      @[<v>\
      @[<v 2>\
      type ('a, 'b) t = {@ \
        hash : 'a -> int;@ \
        eq : 'a -> 'a -> bool;@ \
        mutable length : int;@ \
        mutable values : ('a * 'b) list array;@]@ \
      }@ \
;;@]@ \
    @ \
      @[<v>\
      @[<v 2>\
      let create h eq len =@ \
        @[<v 2>\
        if len <= 0 ||@ \
           len >= Sys.max_array_length@]@ \
        then invalid_arg (Printf.sprintf \"Memo_tbl.create %%d\" len) else@ \
        @[<v 2>\
        let hash x =@ \
          let h_code = h x in@ \
          @[<v 2>\
          if h_code < 0 then@ \
            failwith (Printf.sprintf \"Memo_tbl: bad hashing value %%d\" h_code) else@]@ \
          h_code in@]@ \
        @[<v 2>\
        { hash = hash;@ \
          eq = eq;@ \
          length = len;@ \
          values = Array.make len []; }@]\
      @]@ \
;;@]@ \
    @ \
      @[<v>\
      @[<v 2>\
      let add t x y =@ \
        let h = t.hash x mod t.length in@ \
        t.values.(h) <- (x, y) :: t.values.(h)@]@ \
;;@]@ \
    @ \
      @[<v>\
      @[<v 2>\
      let find t x =@ \
        let h = t.hash x mod t.length in@ \
        let bucket = t.values.(h) in@ \
        @[<v 2>\
        let rec find_in_bucket x = function@ \
          | [] -> raise Not_found@ \
          | (x0, y0) :: rest -> if t.eq x x0 then y0 else find_in_bucket x rest in@]@ \
        find_in_bucket x bucket@]@ \
;;@]@ \
    @]@ \
    end@ \
;;@]@ \
    @ \
    @[<v>\
    @[<v 2>\
    let positive_hash h =@ \
      @[<v 2>\
      let h =@ \
        if h < 0 then -h else h in@ \
        if h < 0 then h - 10 else h@]\
      @]@ \
;;@] @]@."
;;


let genr_memo_tbl tname _ctype cdef =
  match cdef with
  | (_cname, core_type_list, _rels, _loc) ->
    let len = List.length core_type_list in
    (* Nothing need to be done for nullary constructors
       and the following should not happen *)
    if len = 0 then failwith "Trying to memoize a nullary constructor";
    let g = Otype.generator_of_cdef cdef in
    let current_type = Otype.constr_type tname [];
    in
    let _, eq_body, hash_body =
      List.fold_left
        (fun (index, eq_string, hash_string) coretype ->
          let hash_sep = if index = 0 then " " else " + "
          and equal_sep = if index = 0 then " " else " && " in
          let hstring, estring =
            (fun (hash_s, eq_s) ->
              Format.sprintf "%s%s%s" hash_string hash_sep hash_s,
              Format.sprintf "%s%s%s" eq_string equal_sep eq_s)
              (if Otype.eq_type coretype current_type then
                  Format.sprintf "get_hash_t x%d" index,
                Format.sprintf "equal_t x%d y%d" index index
               else Format.sprintf "Hashtbl.hash x%d" index,
                Format.sprintf "( = ) x%d y%d" index index)
          in index + 1, estring, hstring)
        (0, "", "") core_type_list in
    let xlist = mk_tuple_string_with_prefix "x" len
    and ylist = mk_tuple_string_with_prefix "y" len in
    let tbl_body =
      Format.sprintf
        "@[<v>\
      @[<v 2>\
       let hash %s =@ \
        let h = %s in@ \
         positive_hash h@]@ \
       @[<v 2> \
        and eq %s %s =@ \
         %s in@]@ \
        Memo_htbl.create hash eq %d@]"
        xlist hash_body xlist ylist eq_body (get_memo_table_size ())in
    let body_code = Code.Ocaml_expression (Otype.mk_ident tbl_body) in
    let tbl_ident = Genr_base.construction_function_memo_table_ident g in
    Genr_base.Construction_helper (tbl_ident, [], body_code)
;;

let memo_body body g x =
  let result_ident = Lident "result" in
  let tbl = Constant (Genr_base.construction_function_memo_table_ident g) in
  let find_ident = Ldot (Lident "Memo_htbl", "find")
  and add_ident = Ldot (Lident "Memo_htbl", "add") in
  let with_clause =
    Clause
      (make_Constant "Not_found",
       Let (result_ident, [], body,
       Sequence
         (apply3 add_ident tbl x (Constant result_ident),
          Constant result_ident, []))) in
  Try (apply2 find_ident tbl x, [ with_clause ])
;;

let genr_memo_functions tname ctype cdef generated_functions =
  List.fold_left
    (fun fun_acc gen_fun ->
      match gen_fun with
      (* A construction function should only have *one* tuple argument
         in order for it to be memoizable by the current process.
      *)
      | Construction (g, [arg], body) ->
        genr_memo_tbl tname ctype cdef ::
          (Construction(g, [arg], memo_body body g arg) :: fun_acc)
      | Construction _
      | Comparison _
      | Construction_helper _
      | Construction_record _ -> gen_fun :: fun_acc
    ) [] generated_functions

(*
 Local Variables:
  compile-command: "cd ..; make"
 End:
*)

