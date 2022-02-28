(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          FrÅÈdÅÈric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2007,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: tcantor.ml,v 1.3 2007-11-22 15:35:28 weis Exp $ *)

open Peano;;
open Cantor;;
open Cantor_op;;

open Gentest;;

testing "Natdigit";;

let ob = Buffer.create 1000;;

let string_of_cantor x =
  Cantor_op.bprintf ob x;
  let res = Buffer.contents ob in
  Buffer.clear ob;
  res
;;

let p0 = peano_of_int 0;;
let p1 = peano_of_int 1;;

let one = cons (p1, zero, zero);;
let w = cons (p1, one, zero);;

testi 0 ("1" = string_of_cantor one);;
testi 1 ("w" = string_of_cantor w);;
testi 2 ("w.2" = string_of_cantor (add (w, w)));;
testi 3 ("w^2" = string_of_cantor (mul (w, w)));;
