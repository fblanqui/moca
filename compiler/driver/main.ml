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

(* $Id: main.ml,v 1.132 2012-06-04 13:01:22 weis Exp $ *)

(** {3 Driving the mocac compiler and its options.} *)

open Parsetree
open Genr_base
open Genr_sharing
open Genr_mli
open File
;;

exception Usage_error of string;;

let usage s = raise (Usage_error s);;

(* The table of locally open modules. *)

module Str = struct
  type t = string
  let compare = Pervasives.compare
end
;;

module StrSet = Set.Make (Str);;

let add_local_module, is_local_module =
  let module_set = ref StrSet.empty in
  let add_local s = module_set := StrSet.add s !module_set
  and is_local s = StrSet.mem s !module_set in
  add_local, is_local
;;

(* Do we generate an interface ? *)
let get_genr_interface, set_genr_interface =
  let genr_interface = ref false in
  (fun () -> !genr_interface),
  (fun () -> genr_interface := true)
;;

(* Do we generate an implementation ? *)
let get_genr_implementation, set_genr_implementation =
  let genr_implementation = ref false in
  (fun () -> !genr_implementation),
  (fun () -> genr_implementation := true)
;;

(* Do we generate a test file ? *)
let get_genr_test, set_genr_test =
  let genr_test = ref false in
  (fun () -> !genr_test),
  (fun () -> genr_test := true)
;;

(* Seed for generating random tests *)
let get_seed, set_seed =
  let seed =
    Random.self_init ();
    ref (Random.bits ()) in
  (fun () -> !seed),
  (fun s -> seed := s)
;;

(* Number of tests generated per equation. *)
let get_num_tests, set_num_tests =
  let num_tests = ref 5 in
  (fun () -> !num_tests),
  (fun n -> num_tests := n)
;;

(* Maximal constructor depth of each generated value *)
let get_val_depth, set_val_depth =
  let val_depth = ref 3 in
  (fun () -> !val_depth),
  (fun n -> val_depth := n)
;;

(* The output file name for the test file. *)
let get_testing_file_name, set_testing_file_name =
  let testing_file_name = ref "-" in
  (fun () -> !testing_file_name),
  (fun s ->
   if !testing_file_name = "-"
   then testing_file_name := s
   else usage "test file name already set.")
;;

(* The output file name for the implementation of the generated module. *)
let get_output_module_implementation_name,
    set_output_module_implementation_name =
  let output_module_implementation_name = ref "-" in
  (fun () -> !output_module_implementation_name),
  (fun fname ->
   if !output_module_implementation_name = "-"
   then output_module_implementation_name := fname
   else usage "output implementation module name already set.")
;;

(* The output file name for the interface of the generated module. *)
let get_output_module_interface_name, set_output_module_interface_name =
  let output_module_interface_name = ref "-" in
  (fun () -> !output_module_interface_name),
  (fun fname ->
   if !output_module_interface_name = "-"
   then output_module_interface_name := fname
   else usage "output interface module name already set.")
;;

(* The output file name for the generated module. *)
let get_output_module_name, set_output_module_name =
  let output_module_name = ref "-" in
  (fun () -> !output_module_name),
  (fun fname ->
   if !output_module_name = "-" then
     begin
       output_module_name := fname;
       set_output_module_interface_name (!output_module_name ^ ".mli");
       set_output_module_implementation_name (!output_module_name ^ ".ml");
       set_testing_file_name (!output_module_name ^ "_test.ml")
     end
   else usage "output module name already set.")
;;

(* The mlm source file. *)
let get_input_file_name, set_input_file_name =
  let input_file_name = ref "-" in
  (fun () -> !input_file_name),
  (fun fname ->
   if !input_file_name = "-" then input_file_name := fname
   else usage "input file name already set.")
;;

let set_implementation_from_file_name fname =
  set_genr_implementation ();
  set_input_file_name fname
;;

let set_output_file_name fname =
  if get_output_module_interface_name () = "-" then
    set_output_module_interface_name
      (change_mlm_extension "" fname ^ ".mli");
  if get_output_module_implementation_name () = "-" then
    set_output_module_implementation_name
      (change_mlm_extension "" fname ^ ".ml");
  if get_testing_file_name () = "-" then
    set_testing_file_name
      (change_mlm_extension "" fname ^ "_test.ml")
;;

(* The functions that checks the admissibility of a .mlm file
   and generates the corresponding interface and implementation. *)

let check_signature_item s si =
  match si.psig_desc with
  | Psig_type ntds -> Check.add_type_decls (Check.Global s) ntds
  | _ -> ()
;;

let check_structure_item s si =
  match si.pstr_desc with
  | Pstr_type ntds -> Check.add_type_decls (Check.Global s) ntds
  | _ -> ()
;;

let check_and_genr_mli sharing ppf si =
  match si.pstr_desc with
  | Pstr_type ntds ->
    genr_mli sharing ppf ntds;
  | Pstr_module (s, _) -> add_local_module s
  | Pstr_recmodule l ->
    List.iter
      (fun (s, _, _) -> add_local_module s)
      l
  | Pstr_open (Longident.Lident s as lid) when not (is_local_module s) ->
    Format.fprintf ppf "open %a" Pr_ocaml.pr_ident_in_prefix_position lid;
    Pr_ocaml.pr_structure_sep ppf
  | _ -> ()
;;

let check_and_genr_ml sharing ppf si =
  let print_si () =
    Pr_ocaml.pr_verbatim_structure_item ppf si;
    Pr_ocaml.pr_structure_sep ppf in

  let check_and_gen ppf = function
    | Pstr_type ntds ->
      Check.add_type_decls Check.Local ntds;
      if sharing then
        Genr_sharing.print_sharing ppf ntds else
      Genr_mli.genr_public_type_declarations sharing ppf ntds;
      if Genr_memo.get_memoize_target () then
        Genr_memo.print_memo_hash_table_module ppf;
      let structure_items = Genr.genr_functions ntds in
      Pr_code.pr_structure ppf structure_items
    | Pstr_module (s, _) ->
      print_si ();
      add_local_module s
    | Pstr_recmodule l ->
      print_si ();
      List.iter (fun (s, _, _) -> add_local_module s) l
    | Pstr_open (Longident.Lident s as li) ->
      begin
        print_si ();
        if not (is_local_module s) then
        try
          let fn = search_mli_file_for_module s in
          let sis = parse_mli_file fn in
          List.iter (check_signature_item s) sis with
        | Not_found ->
          try
            let fn = search_ml_file_for_module s in
            let sis = parse_ml_file fn in
            List.iter (check_structure_item s) sis with
          | Not_found ->
              Check.raise_error si.pstr_loc (Check.Cannot_find_module li)
      end
    | _ -> print_si () in

  Format.fprintf ppf "@[<v>%a@]" check_and_gen si.pstr_desc;
;;

let genr_test seed si =
  match si.pstr_desc with
  | Pstr_type ntds ->
    Genr_testing.genr_type_test
      seed (get_num_tests ()) (get_val_depth ()) ntds
  | Pstr_open (Longident.Lident s as lid) when not (is_local_module s) ->
    [ Code.Pstr_open lid; ]
  | _ -> []
;;

(* Parsing of the current mlm file and generation of the corresponding
   interface and implementation files. *)

let genr_file print_sharing_preamble check_and_genr sis oname =
  let oc = File.careful_open_out oname in
  let ppf = Format.formatter_of_out_channel oc in
  Format.pp_set_mark_tags ppf true;
  Pr_code.set_output_filename oname;
  Pr_code.reset_line_count ();
  let out, flush, outnewline, outspace =
    Format.pp_get_all_formatter_output_functions ppf () in
  let outnewline () =
(*    let lc = Pr_code.get_line_count () in
      Format.fprintf ppf "@{<%d> @}" lc; *)
    Pr_code.incr_line_count ();
    outnewline () in
  Format.pp_set_all_formatter_output_functions
    ppf ~out ~flush ~newline:outnewline ~spaces:outspace;
  let sharing = get_sharable_target () in
  let print_sharing =
    if sharing then print_sharing_preamble else ignore in
  Format.fprintf ppf "@[<v 0>%t%a@]%!"
    print_sharing
    (fun ppf -> List.iter (check_and_genr sharing ppf)) sis;
  File.careful_close_out oc;
;;

let genr_file_mli sis oname =
  if Configuration.get_verbose () then
    print_string "Generating interface ...\n";
  genr_file
    Genr_sharing.print_sharing_mli_preamble
    check_and_genr_mli
    sis oname
;;

let genr_file_ml sis oname =
  if Configuration.get_verbose () then
    print_string "Generating implementation ...\n";
  genr_file
    Genr_sharing.print_sharing_ml_preamble
    check_and_genr_ml
    sis oname
;;

let genr_test_file sis oname =
  if Configuration.get_verbose () then
    print_string "Generating test file ...\n";
  let oc = File.careful_open_out oname in
  let ppf = Format.formatter_of_out_channel oc in
  let seed = get_seed () in
  let module_name =
    module_of_mli_file (get_output_module_interface_name ()) in
  let preamble = Genr_testing.genr_test_preamble module_name seed in
  let structure_items =
    List.fold_left
      (fun code_list si -> code_list @ genr_test seed si) [] sis in
  Pr_code.pr_structure ppf (preamble @ structure_items);
  Format.fprintf ppf "%!";
  File.careful_close_out oc;
;;

let genr fname =
  try

    if Filename.check_suffix fname ".mlms" then
      begin
        if Configuration.get_verbose () then
          print_string ".mlms input file : using sharing ...\n";
        set_sharable_target ();
      end;

    let sis = parse_ml_file fname in

    begin
      match get_genr_interface (), get_genr_implementation () with
      | true, true
      | false, false ->
          genr_file_mli sis (get_output_module_interface_name ());
          genr_file_ml sis (get_output_module_implementation_name ())
      | true, false ->
          genr_file_mli sis (get_output_module_interface_name ())
      | false, true ->
          genr_file_ml sis (get_output_module_implementation_name ())
    end;

    if get_genr_test () then genr_test_file sis (get_testing_file_name ())

  with
  | Syntaxerr.Error e ->
    Format.fprintf Format.err_formatter "%a@." Syntaxerr.report_error e;
    exit 2

  | Check.Error (loc, e) ->
    Location.print Format.err_formatter loc;
    Format.fprintf Format.err_formatter
      "Checking error: %a@." Check.report_error e;
    exit 2
;;

(* Parsing the command line. *)

let umsg = "Usage: mocac [options] <.mlm[s] file>";;

let print_moca_version v =
  prerr_endline
   (Printf.sprintf
      "The Moca relational types compiler, version %s" v);
  exit 0
;;

let print_moca_short_version () =
  print_moca_version
    (Printf.sprintf "%.1f.%i" Configuration.moca_version_number
    Configuration.moca_minor_version_number)
;;

let print_moca_full_version () =
  print_moca_version Configuration.moca_full_version
;;

let rec argspec =  [
  ("-c", Arg.String set_implementation_from_file_name,
   " generate a module for the input file argument");
  ("-comments", Arg.Unit Genr_base.set_comments,
   " add comments to trace the origin of generated clauses");
  ("-d", Arg.Unit Debug.set_debug,
   " debugging mode");
  (* ("-dlinetrace", Arg.Unit Genr_base.set_dline_trace,
   *  " add diese line directives to produced .ml files"); *)
  ("-h", Arg.Unit print_usage,
   " print this option list and exit");
  ("-help", Arg.Unit print_usage,
   " print this option list and exit");
  ("-I", Arg.String add_dir_to_search_path,
   "<dir>   add a directory to the search path");
  ("-i", Arg.Unit set_genr_interface,
   " generate a module interface for the input file argument");
  ("-kb", Arg.Unit Eqnrel.set_kb,
   " set completion on (experimental option under development)");
  ("-kb-limit", Arg.Int Eqnrel.set_kb_limit,
   "<int>   set the upper bound to completion steps");
  ("-memo", Arg.Unit Genr_memo.set_memoize_target,
   " generate memoizing construction functions");
  ("-memo-hash-table-size", Arg.Int Genr_memo.set_memo_table_size,
   "<int>   set the size of memoization tables.");
  ("-ntests", Arg.Int set_num_tests,
   "<int>   set the number of generated tests per equation\
  \n      (use with -test)");
  ("-o", Arg.String set_output_module_name,
   "<file>   output the module with name the file name argument");
  ("-oml", Arg.String set_output_module_implementation_name,
   "<file>   output the module implementation in the file argument");
  ("-omli", Arg.String set_output_module_interface_name,
   "<file>   output the module interface in the file argument");
  ("-otest", Arg.String set_testing_file_name,
   "<file>   output tests in the file argument");
  ("-seed", Arg.Int set_seed,
   "<int>   set the seed for random tests\
  \n      (use with -test)");
  ("-sharing", Arg.Unit set_sharable_target,
   " generate maximally sharing data structures");
  ("-test", Arg.Unit set_genr_test,
   " generate a file for testing the code generated by Moca");
  ("-urorder", Arg.Unit Genr_base.set_user_rel_order,
   " use the relation order provided in the source file");
  ("-valdepth", Arg.Int set_val_depth,
   "<int>   set the maximal constructor depth of the values \
  \n      generated for testing (to be used with -test)");
  ("-verbose", Arg.Unit Configuration.set_verbose,
   " be verbose during generation");
  ("-v", Arg.Unit print_moca_short_version,
   " print version string and exit");
  ("-version", Arg.Unit print_moca_full_version,
   " print full version string and exit");
]

and print_usage () =
  Arg.usage (Arg.align argspec) umsg;
  exit 0;
;;

(* The main procedure *)

let main () =
  try
    Arg.parse (Arg.align argspec) set_input_file_name umsg;
    let fname = get_input_file_name () in
    set_output_file_name fname;
    genr fname;
    exit 0 with
  | Usage_error s ->
    Format.fprintf Format.err_formatter "Usage error: %s@." s;
    exit 2
  | Unknown_file_extension s ->
    Format.fprintf Format.err_formatter
      "Usage error: file ``%s'' has no known extension.@." s;
    exit 3
  | Failure s ->
    Format.fprintf Format.err_formatter "Aborting: spurious failure %s.@." s;
    exit 4
  | x ->
    Format.fprintf Format.err_formatter
      "Aborting: spurious exception %s@."
      (Printexc.to_string x);
    exit 5
;;

(* Run the main procedure. *)

if !Sys.interactive then () else main ()
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
