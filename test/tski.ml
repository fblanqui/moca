(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Fr?d?ric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2012,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: tski.ml,v 1.4 2012-06-04 13:01:22 weis Exp $ *)

(* Testing combinatory logic. *)

open Gentest;;

open Ski;;

testing "Ski";;

testi 0 (i = app (app (s, k), i));;

testi 1 (
 let f = app (app (s, k), i) in
 (app (f, s), app (f, k), app (f, i)) =
 (s, k, i));;
