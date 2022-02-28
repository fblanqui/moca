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

(* $Id: modint.ml,v 1.3 2007-11-21 12:49:59 blanqui Exp $ *)

(* integers modulo *)

open Natge;;

module Make (R : sig val radix : Natge2.t end) =
struct

  type t = Mk of int;;

  let r = Natge2.value R.radix;;

  let mk x =
    if x >= 0 && x < r then Mk x
    else failwith ("Integer not between 0 and " ^ string_of_int r)
  ;;

  let value (Mk x) = x;;

  let add_table = Array.init r (fun i -> Array.init r (fun j -> (i+j) mod r));;

  let add (Mk x) (Mk y) = Mk (add_table.(x).(y));;

  let prod_table = Array.init r (fun i -> Array.init r (fun j -> (i*j) mod r));;

  let prod (Mk x) (Mk y) = Mk (prod_table.(x).(y));;

end;;
