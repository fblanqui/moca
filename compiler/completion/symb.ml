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

(* symbols *)

type symbol = int;;

open Useful;;

module SymbolSet = IntSet;;
module SymbolMap = IntMap;;

let symbol_of_string, string_of_symbol, clear_symbols =
  let symap = ref SymbolMap.empty
  and stmap = ref StrMap.empty
  and n = ref (-1) in
    (fun s ->
       (try StrMap.find s !stmap
        with Not_found ->
          incr n;
          stmap := StrMap.add s !n !stmap;
          symap := SymbolMap.add !n s !symap;
          !n)),
    (fun n -> SymbolMap.find n !symap),
    (fun () ->
       symap := SymbolMap.empty;
       stmap := StrMap.empty;
       n := 0)
;;

let pr_symbol ppf symb =
  Format.fprintf ppf "%s" (string_of_symbol symb)
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
