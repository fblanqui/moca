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

(* $Id: nqueens.ml,v 1.4 2009-04-22 10:00:26 weis Exp $ *)

open Bdd;;
open Format;;

(* i : row
   j : column
   n : number of queens
*)
let var i j = Printf.sprintf "x%d%d" i j
;;

let make_at i j = 
  Bdd.batom (var i j)
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
 
let make_constr i j n =
  assert (n >= 1);
  let rec aux iter =
    if iter > n then btrue else 
    let v = vert_constr i j iter 
    and h = horiz_constr i j iter
    and d = diag_constr i j iter n in
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

let rec solution = function
  | Bp ( _, x, Bfalse, b1) -> solution b1
  | Bp (_, x, b1, b2) -> 
    let xsolutions =
      List.map (fun y -> x :: y) (solution b1)
    and notxsolutions = solution b2 in
    xsolutions @ notxsolutions
  | Btrue ->  [[]]
  | Bfalse -> []
  | _ -> failwith "Uh Uh\n"
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
        List.iter (fun y -> Format.fprintf ppf "@[%s@ @]" y) x;
        Format.fprintf ppf "@.End solution@.";    
      ) l
;;

let get_all_solutions ppf n =
  Format.fprintf ppf "Solutions for %d queens@." n;
  print_solutions ppf (model_path (make_queens n))
;;
