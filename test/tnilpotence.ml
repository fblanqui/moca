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

(* $Id: tnilpotence.ml,v 1.2 2007-11-21 10:23:31 weis Exp $ *)

(* Testing nilpotence. *)

open Gentest;;

open Nilpotence;;

testing "Nilpotence";;

testi 0 (proj (proj zero) = one);;
testi 1 (proj (proj one) = one);;
testi 2 (nprod (zero, nprod (zero, zero)) = zero);;
testi 3 (nprod (one, nprod (one, one)) = nprod (one, zero));;
testi 4 (proj (proj (nprod (nprod (one, one), nprod (one, one)))) = one);;
