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

(* $Id: queens.ml,v 1.2 2009-04-22 09:49:32 weis Exp $ *)

open Bdd;;
open Format;;

let debug = ref false;;

let rec print_bdd ppf = function
  | Bdd.Btrue -> fprintf ppf "true"
  | Bdd.Bfalse -> fprintf ppf "false"
  | Bdd.Bp (_, p, x, y) ->
    fprintf ppf "@[if@ %s@ (%a, %a)@]" (Atom.name p) print_bdd x print_bdd y
  | Bdd.Bnot (_, x) -> fprintf ppf "~%a" print_bdd x
  | Bdd.Band (_, x, y) ->  fprintf ppf "@[%a@ /\\@ %a@]" print_bdd x print_bdd y
  | Bdd.Bor (_, x, y) -> fprintf ppf "@[%a \\/@.%a@]" print_bdd x print_bdd y
  | Bdd.Bxor (_, x, y) -> fprintf ppf "@[%a +@.%a@]" print_bdd x print_bdd y
  | _ -> assert false
;;

(* i : row
   j : column
   n : number of queens
*)
let var i j = Printf.sprintf "x%d%d" i j
;;

let make_at i j =
  Bdd.batom (Atom.make (var i j))
;;

let make_neg_lit i j =
  Bdd.bnot (make_at i j)
;;

let vert_constr i j iter =
  if iter != j then make_neg_lit i iter else Bdd.btrue
;;

let horiz_constr i j iter =
  if iter != i then make_neg_lit iter j else btrue
;;

let diag_constr i j iter n =
  let between inf sup x =
    x >= inf && x <= sup in
  let ind1 = j + iter - i in
  let ind2 = j + i - iter in
  if iter = i then btrue else
    match between 1 n ind1, between 1 n ind2 with
    | true, true ->
      Bdd.band (make_neg_lit iter ind1, make_neg_lit iter ind2)
    | true, false -> make_neg_lit iter ind1
    | false, true -> make_neg_lit iter ind2
    | false, false -> btrue
;;

let trace_bdd bdd =
  Format.eprintf "%a@." print_bdd bdd
;;

let make_constr i j n =
  assert (n >= 1);
  let rec aux iter =
    if iter > n then btrue else
    let v = vert_constr i j iter
    and h = horiz_constr i j iter
    and d = diag_constr i j iter n in
    if !debug then begin
      trace_bdd v;
      trace_bdd h;
      trace_bdd d;
    end;
    band (v, band (h , band (d, aux (iter + 1)))) in
  aux 1
;;

let make_row i n =
  assert (n >= 1);
  let rec aux iter =
    if iter > n then bfalse else
    band
      (bimplies (make_at i iter, make_constr i iter n),
       bor (make_at i iter, aux (iter + 1))) in
  aux 1
;;

let make_queens n =
  assert (n >= 1);
  let rec aux iter =
    if iter > n then btrue else
    band (make_row iter n, aux (iter + 1)) in
  aux 1
;;

(*
No memo.

let rec solution = function
  | Bp (_, x, Bfalse, b1) -> solution b1
  | Bp (_, x, b1, b2) ->
    let xsolutions =
      List.map (fun y -> x :: y) (solution b1)
    and notxsolutions = solution b2 in
    xsolutions @ notxsolutions
  | Btrue ->  [[]]
  | Bfalse -> []
  | _ -> failwith "Uh Uh\n"
;;
*)

(*
Memo with equal.

let solutions_table = Hashtbl.create 100;;

let rec solution b =
  try Hashtbl.find solutions_table b with
  | Not_found ->
    let sols =
      match b with
      | Bp (_, x, Bfalse, b1) -> solution b1
      | Bp (_, x, b1, b2) ->
        let sol1 = solution b1 in
        let sol2 = solution b2 in
        let xsolutions =
          List.map (fun y -> x :: y) sol1
        and notxsolutions = sol2 in
        xsolutions @ notxsolutions
      | Btrue ->  [[]]
      | Bfalse -> []
      | _ -> failwith "Uh Uh\n" in
    Hashtbl.add solutions_table b sols;
    sols
;;

*)

(* Memo with eq. *)
module Htbl = Hashtbl.Make (struct
  type t = Bdd.t;;
  external equal : t -> t -> bool = "%eq"
  external hash_param : int -> int -> 'a -> int =
    "caml_hash_univ_param" "noalloc";;
  let hash x = hash_param 10 100 x;;
 end);;

let solutions_table = Htbl.create 100;;

let rec solution b =
  try Htbl.find solutions_table b with
  | Not_found ->
    let sols =
      match b with
      | Bp (_, x, Bfalse, b1) -> solution b1
      | Bp (_, x, b1, b2) ->
        let sol1 = solution b1 in
        let sol2 = solution b2 in
        let xsolutions =
          List.map (fun y -> x :: y) sol1
        and notxsolutions = sol2 in
        xsolutions @ notxsolutions
      | Btrue ->  [[]]
      | Bfalse -> []
      | _ -> failwith "Uh Uh\n" in
    Htbl.add solutions_table b sols;
    sols
;;

let model_path = solution
;;

let print_solutions ppf l =
  match l with
  | [] -> Format.fprintf ppf "No Solution@."
  | _ ->
    let c = ref 0 in
    List.iter
      (fun x ->
        c := !c + 1;
        Format.fprintf ppf "Solution %d@." !c;
        List.iter (fun y -> Format.fprintf ppf "@[%s@ @]" (Atom.name y)) x;
        Format.fprintf ppf "@.End solution@.";
      )
      l
;;

let get_all_solutions ppf n =
  Format.fprintf ppf "Solutions for the %d queens problem:@." n;
  let bdd = make_queens n in
  if !debug then begin
    print_bdd Format.err_formatter bdd;
    fprintf Format.err_formatter "@.";
  end;
  print_solutions ppf (model_path bdd)
;;
