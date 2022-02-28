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

(* $Id: main.ml,v 1.6 2009-04-22 10:00:26 weis Exp $ *)

open Pprint;;
open Bdd;;
open Nqueens;;

let bimplies (a, b) = bor (bnot a, b);;
let v = batom "v"
and w = batom "w"
and x = batom "x"
and y = batom "y"
and z = batom "z"
and a = batom "a"
and b = batom "b"
and c = batom "c"
;;

let noty = bnot y
and notx = bnot x
;;

let equiv p q = band (bimplies (p, q), bimplies (q, p));;

(* let p = bnot (band (bor (noty, x), bor (notx, y)));; *)
let rec gen_sym n =
  if n = 1 then [batom ("x" ^ string_of_int n)]
  else batom ("x" ^ string_of_int n) :: gen_sym (n - 1)
;;

let equiv_test n =
  let symb_list = gen_sym n in
  let d_symb_list = symb_list @ symb_list in
  List.fold_right
    (fun x y -> equiv x y)
    (List.tl d_symb_list)
    (List.hd d_symb_list)
;;


let h = bimplies (bimplies (x, y), x);;
let peirce = bimplies (h, x);;

(* Let p2 =  bor (band (x,y), y);; *)
(* let p1 = bnot ( bor (band (x,y), y));; *)

(* let q = band (bor (x, band (w,v)), z);; *)
(* let q2 = bnot q;; *)

(* let s = band (bor (x, z), bor (y,z));; *)
(* let s1 = bor (q, s);; *)

let ex_mid = bor (x, notx);;

let x = Dt.batom "x" and y = Dt.batom "y";;
let pdt = Dt.band (x, y);;

let x = Bdd.batom "x" and y = Bdd.batom "y";;
let pbdd = Bdd.band (x, y);;

let mytauto =
  [ (peirce, "Peirce's Law");
    ( ex_mid, "Excluded Middle");
    ( (bimplies
        (bimplies (b, c),
         bimplies
           (bimplies (a, b),
            bimplies (a, c))))
       , "B Axiom" );
    ( (bimplies
        (bimplies (a, bimplies (b, c)),
         bimplies (b, bimplies (a, c))))
       , "C Axiom");
    ( (bimplies
        (bimplies (a, bimplies (a, b)),
         bimplies (a, b)))
       , "W Axiom");
    ( (bimplies
        (bimplies (bnot (bnot a), bnot (bnot b)),
         bnot (bnot (bimplies (a, b)))))
       , "TS 2.1.8D");
  ]
;;

let test n f  =
  print_string ("\n**** " ^ n ^ " ****\n");
  Pprint.pout_bdd f;
  flush_all ();

  match f with
  | Btrue -> print_string "\n \\o/\\o/\\o/ Proof Found \n"
  | _ -> print_string "\n :-((((( Proof NOT Found\n";
  flush_all ();
;;

let testlist = List.iter (fun (f, n) -> test n f );;

let umsg = "Usage: nqueens <n>";;
let argspec = [];;
let queen s =
  let ppf = Format.std_formatter in
  Nqueens.get_all_solutions ppf (int_of_string s)
;;

let main () =
  try
    Arg.parse (Arg.align argspec) queen umsg
  with _ -> print_string "Error\n"
(*   testlist [ (make_queens 1, "1 queen") ];  *)
(*   testlist [ (make_queens 2, "2 queens") ];  *)
(*    testlist [ (make_queens 3, "3 queens") ];   *)
(* (\*     testlist [ (make_queens 4, "4 queens") ];  *\) *)
(* (\*   testlist [((make_queens 5), "5 queens") ];  *\) *)
(*   (\* testlist [(Nqueens.make_queens 4, "4queens") ]; *\) *)
(*   get_all_solutions 1; *)
(*   get_all_solutions 4; *)
(*   get_all_solutions 5; *)
(*   get_all_solutions 6; *)
(*   get_all_solutions 7; *)
;;

main ()
;;
