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

(* term data structure *)

open Var;;
open Symb;;

type term =
  | Var of var
  | App of symbol * term list
;;

let rec pr_term ppf term =
  match term with
    | Var v -> Format.fprintf ppf "%a" Var.pr_var v
    | App (s, tlist) ->
      if tlist <> [] then
        Format.fprintf ppf "@[<hov 2>%a@ (%a)@]"
          Symb.pr_symbol s pr_term_list tlist
      else Format.fprintf ppf "%a" Symb.pr_symbol s

and pr_term_list ppf termlist =
  match termlist with
    | term :: [] ->
      Format.fprintf ppf "%a" pr_term term
    | term :: terms ->
      Format.fprintf ppf "%a,@ %a" pr_term term pr_term_list terms
    | [] -> assert false
;;

let rec vars = function
  | Var x -> VarSet.singleton x
  | App (_, ts) -> vars_of_terms ts

and vars_of_terms = function
  | [] -> VarSet.empty
  | t :: ts -> VarSet.union (vars t) (vars_of_terms ts)
;;

let is_linear =
  let vset = ref VarSet.empty in
  let rec aux = function
    | Var x ->
        begin
          match VarSet.mem x !vset with
            | true -> false
            | false -> vset := VarSet.add x !vset; true
        end
    | App (_, ts) -> List.for_all aux ts
  in
    fun t ->
      let b = aux t in
      vset := VarSet.empty; b
;;

let rec symbols = function
  | Var _ -> SymbolSet.empty
  | App (f, ts) -> SymbolSet.add f (symbols_of_terms ts)

and symbols_of_terms = function
  | [] -> SymbolSet.empty
  | t :: ts -> SymbolSet.union (symbols t) (symbols_of_terms ts)
;;

let occurs x =
  let rec aux = function
    | Var y -> y = x
    | App (_, ts) -> List.exists aux ts
  in aux
;;

let rec fprintf ppf = function
  | Var x -> Format.fprintf ppf "%s" (raw_string_of_var x)
  | App (f, []) -> Format.fprintf ppf "%s" (string_of_symbol f)
  | App (f, ts) -> Format.fprintf ppf "(%s%a)" (string_of_symbol f) fprintf_terms ts

and fprintf_terms ppf = List.iter (Format.fprintf ppf " %a" fprintf)
;;

let rename imap =
  let rec aux = function
    | Var x as t -> (try Var (VarMap.find x imap) with Not_found -> t)
    | App (f, ts) -> App (f, List.map aux ts) in
  aux
;;

(* canonical representation of a term *)

let rec list_vars acc = function
  | Var x -> x :: acc
  | App (_, ts) -> list_vars_of_terms acc ts

and list_vars_of_terms acc = function
  | [] -> acc
  | t :: ts -> list_vars_of_terms (list_vars acc t) ts
;;

let list_vars = list_vars [];;
let list_vars_of_terms = list_vars_of_terms [];;

let canonical t =
  let xs = list_vars t in
  let rec aux = function
    | Var x -> Var (Mylist.position x xs)
    | App (f, ts) -> App (f, List.map aux ts)
  in aux t
;;

let compare t u = Pervasives.compare (canonical t) (canonical u);;

let eq t u = compare t u = 0;;

(* linearize a term *)

let linearize =

  let init, get_imap, fresh =
    let v = ref 0 and vset = ref VarSet.empty and imap = ref VarMap.empty in
    (fun () -> v := 0; vset := VarSet.empty; imap := VarMap.empty),
    (fun () -> !imap),
    (fun x ->
      if VarSet.mem x !vset
      then (incr v; imap := VarMap.add !v x !imap; Var !v)
      else (vset := VarSet.add x !vset; Var x)) in

  let rec aux = function
    | Var x -> fresh x
    | App (f, ts) -> App (f, List.map aux ts) in

  fun t ->
    let t = aux t in
    let imap = get_imap () in
    init ();
    t, imap
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
