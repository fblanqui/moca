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

(* Tests for morebool.mlm : a simple commutative structure *)

open Morebool;;
open Gentest;;

testing "Morebool";;

let a = batom "a"
and b = batom "b"
and c = batom "c"
;;

let b1 = band (a,  b)
and b2 = band (b, a);;

let b3 = band (c, b1)
and b4 = band (b2, c);;

let b5 = band (a, band (b, c))
and b6 = band (b, band (a, c));;

testi 0 (b1 = b2);;
testi 1 (b3 = b4);;
testi 2 (b5 <> b6);;
