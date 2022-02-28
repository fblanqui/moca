(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: peano.ml,v 1.3 2008-02-11 19:37:34 weis Exp $ *)

type t = Zero | S of t;;

let rec peano_of_int = function
  | n when n > 0 -> S (peano_of_int (pred n))
  | _ -> Zero;;

let rec add x = function
  | Zero -> x
  | S y -> add (S x) y;;

let mul x =
  let rec aux = function
    | Zero -> Zero
    | S y -> add (aux y) x
  in aux;;

let rec compare x y =
  match x, y with
  | Zero, Zero -> 0
  | _, Zero -> 1
  | Zero, _ -> -1
  | S x, S y -> compare x y;;

let rec minus x y =
  match x, y with
  | Zero, Zero
  | Zero, S _ -> Zero
  | S _, Zero -> x
  | S x, S y -> minus x y;;

type comp = Eq | Lt of t | Gt of t;;

let rec compare_minus x y =
  match x, y with
  | Zero, Zero -> Eq
  | S _, Zero -> Gt x
  | Zero, S _ -> Lt y
  | S x, S y -> compare_minus x y;;

open Natge;;

module R = struct let radix = Natge2.mk 10 end;;

module M = Natdigit_by_hand.Make (R);;

open M;;

let rec decimal_of_peano = function
  | Zero -> empty
  | S y -> M.succ (decimal_of_peano y);;

let bprintf ob x = M.bprintf ob (decimal_of_peano x);;
