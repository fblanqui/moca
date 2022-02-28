(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: norm_ml.ml,v 1.14 2012-04-02 09:27:25 weis Exp $ *)

open Rule;;
open Term;;
open Printf;;
open Symb;;
open Check;;
open Var;;
open Configuration;;



let moca_normalization_file = "moca_norm.ml"
and moca_init_test_file = "moca_init.ml"
and moca_outfile = "moca_term_test.out"
and moca_binary_exec = "normalize"
;;

(* Print the ocaml code for defining the type "t" corresponding to a
   set of symbols
*)

let bprint_type_cons ob f =
  let s = string_of_symbol f in
  let symbol_arity = arity_of_info (find_generator_name_info s) in
  match symbol_arity  with
    | 0 -> bprintf ob "| %s " s
    | 1 -> bprintf ob "| %s of t" s
    | _ ->
      bprintf ob "| %s of t" s;
      for _i = 2 to symbol_arity do
        bprintf ob " * t"
      done
;;

let bprint_type ob symbs =
  bprintf ob "type t = | Moca of int";
  SymbolSet.iter (bprint_type_cons ob) symbs;
  bprintf ob "\n"
;;

(* print the ocaml code for defining the normalization function on "t"
   corresponding to a set of rules *)

let rec bprint_pattern ob = function
  | Var i -> bprintf ob "x%i" i
  | App (f, ts) ->
    bprintf ob "%s%a" (string_of_symbol f) bprint_patterns ts

and bprint_patterns ob = function
  | [] -> ()
  | [t] -> bprintf ob " %a" bprint_pattern t
  | t :: ts ->
    bprintf ob "(%a%a)" bprint_pattern t bprint_patterns_comma ts

and bprint_patterns_comma ob = function
  | [] -> ()
  | t :: ts ->
    bprintf ob ",%a%a" bprint_pattern t bprint_patterns_comma ts
;;

let rec bprint_conditions ob = function
  | [] -> ()
  | (x, y) :: cs ->
    bprintf ob " when x%i = x%i%a" x y bprint_conditions_and cs

and bprint_conditions_and ob = function
  | [] -> ()
  | (x, y) :: cs ->
    bprintf ob " && x%i = x%i%a" x y bprint_conditions_and cs
;;

let bprint_rule ob (Rule.Mk (l,r)) =
  let l, vmap = linearize l in
  let cs = VarMap.fold (fun x y l -> (x,y) :: l) vmap [] in
    bprintf ob "\n| %a%a -> %a"
      bprint_pattern l bprint_conditions cs bprint_pattern r
;;

let bprint_norm ob rs =
  bprintf ob "\nlet rec norm = function";
  RulSet.iter (bprint_rule ob) rs;
  bprintf ob "\n| x -> x\n"
;;

(* print the ocaml code for defining a printing function on "t" *)

let bprint_cons_pat ob = function
  | 1 -> bprintf ob " x1"
  | n when n > 0 ->
      begin
        bprintf ob "(x1";
        for i = 2 to n do bprintf ob ",x%i" i done;
        bprintf ob ")"
      end
  | _ -> ()
;;

let bprint_cons_fmt ob = function
  | 1 -> bprintf ob " %%a"
  | n when n > 0 ->
    bprintf ob "(%%a";
    for _i = 2 to n do bprintf ob ",%%a" done;
    bprintf ob ")"
  | _ -> ()
;;

let bprint_cons ob f =
  let s = string_of_symbol f in
  let n = arity_of_info (find_generator_name_info s) in
  bprintf ob "\n| %s%a -> Printf.bprintf ob \"%s%a\""
    s bprint_cons_pat n s bprint_cons_fmt n;
  for i = 1 to n do bprintf ob " bprint x%i" i done
;;

let bprint_print ob symbs =
  bprintf ob "\nlet rec bprint ob = function";
  SymbolSet.iter (bprint_cons ob) symbs;
  bprintf ob "\n| _ -> ()\n"
;;

(* print the ocaml code for creating a buffer *)

let bprint_buffer ob = bprintf ob "\nlet ob = Buffer.create 1000"
;;

(* buffer *)

let ob = Buffer.create 1000
;;

(* write the contents of "ob" in "oc" and close "oc" *)

let write_and_close oc =
  Buffer.output_buffer oc ob;
  Buffer.clear ob;
  close_out oc
;;

(* create the file moca_norm.ml *)

let write_norm_file =
  let n = ref 0 in fun rs -> incr n;
    verbose_print "Create %s (%i) ...@." moca_normalization_file !n;
  let symbs = symbols_of_rules rs
  and oc = open_out moca_normalization_file in
  bprint_type ob symbs;
  bprint_norm ob rs;
  bprint_print ob symbs;
  bprint_buffer ob;
  write_and_close oc
;;

(* create the file moca_term.ml *)

let rec bprint_term ob = function
  | Var i -> bprintf ob "Moca %i" i
  | App (f, ts) -> bprintf ob "%s%a" (string_of_symbol f) bprint_terms ts

and bprint_terms ob = function
  | [] -> ()
  | [t] -> bprintf ob " %a" bprint_term t
  | t :: ts -> bprintf ob "(%a%a)" bprint_term t bprint_terms_comma ts

and bprint_terms_comma ob = function
  | [] -> ()
  | t :: ts -> bprintf ob ",%a%a" bprint_term t bprint_terms_comma ts
;;

let write_term_file t =
  verbose_print "Create %s ...@." moca_init_test_file ;
  let oc = open_out moca_init_test_file in
  bprintf ob
    "open Moca_norm;;\
   \nlet x = %a;;\
   \nbprint ob (norm x);;\
   \n\
   \nlet oc = open_out \"%s\";;\
   \nBuffer.output_buffer oc ob;;\
   \nclose_out oc;;\
   \nBuffer.clear ob;;\n\
    " bprint_term t moca_outfile;
  write_and_close oc
;;

(* read a term from the file moca_term.out *)

let read_term_file () =
  verbose_print "Read %s ...@." moca_outfile;
  let ic = open_in moca_outfile  in
  let lb = Lexing.from_channel ic in
  let t = Cparser.top_term Clexer.next_token lb in
  close_in ic;
  t
;;

(* run a system command *)
let run_cmd s =
  Format.printf "Moca running command: %s@." s;
  ignore (Sys.command s)
;;

(* normalization function *)

let norm =
  let rs_ref = ref RulSet.empty in
  fun rs ->
    if rs != !rs_ref then
      begin
        write_norm_file rs;
        verbose_print "Compile %s ...@." moca_normalization_file;
        let cmd = Format.sprintf "ocamlc %s" moca_normalization_file in
        run_cmd cmd;
        rs_ref := rs
      end;
    fun t ->
      write_term_file t;
      verbose_print "Compile %s ...@." moca_init_test_file;
      let normalization_cmofile =
        (Filename.chop_suffix moca_normalization_file ".ml") ^ ".cmo" in
      let cmd = Format.sprintf "ocamlc -o %s %s %s"
        moca_binary_exec normalization_cmofile moca_init_test_file in
      run_cmd cmd;
      let cmd = Format.sprintf "./%s" moca_binary_exec in
      run_cmd cmd;
      read_term_file ()
;;


(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
