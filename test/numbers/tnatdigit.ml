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

(* $Id: tnatdigit.ml,v 1.5 2007-11-22 15:35:28 weis Exp $ *)

open Natge;;
open Natdigit_by_hand;;

open Decimal;;

open Gentest;;

testing "Natdigit";;

let ob = Buffer.create 1000;;

let string_of_nat n =
  bprintf ob n;
  let res = Buffer.contents ob in
  Buffer.clear ob;
  res
;;

let nat n = of_nat (Nat.mk n);;

testi 0 ("0" = string_of_nat (nat 0));;
testi 1 ("8" = string_of_nat (nat 8));;
testi 2 ("12" = string_of_nat (nat 12));;
testi 3 ("2007" = string_of_nat (nat 2007));;

testi 4 ("20" = string_of_nat (add (nat 8) (nat 12)));;
testi 5 ("64" = string_of_nat (prod (nat 8) (nat 8)));;
