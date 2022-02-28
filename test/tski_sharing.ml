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

(* $Id: tski_sharing.ml,v 1.3 2008-02-11 19:37:33 weis Exp $ *)

(* Testing combinatory logic. *)

open Gentest;;

open Ski_sharing;;

testing "Ski_sharing";;

testi 0 (i = app (app (s, k), i));;

testi 1 (
 let f = app (app (s, k), i) in
 (app (f, s), app (f, k), app (f, i)) =
 (s, k, i));;
