(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Fr?d?ric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2008,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: tlength.ml,v 1.3 2008-02-11 19:37:33 weis Exp $ *)

(* Testing booleans. *)

open Gentest;;

open Length;;

testing "Length";;

let l1 = make_t 1;;

let l2 = make_t 2;;

testi 0 (3 = from_t l1 + from_t l2);;
testi 1 (0 = (1 + from_t l1) mod from_t l2);;
