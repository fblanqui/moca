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

(* $Id: pprint.ml,v 1.3 2009-04-22 10:00:26 weis Exp $ *)

open Format;;

let rec p_dt ppf = function
  |  Dt.Btrue -> fprintf ppf "true"
  |  Dt.Bfalse -> fprintf ppf "false"
  |  Dt.Bp (p, x, y) -> fprintf ppf "@[%s(%a, %a)@]" p p_dt x p_dt y
  |  Dt.Bnot x -> fprintf ppf "~%a" p_dt x 
  |  Dt.Band (x, y) ->  fprintf ppf "@[%a@ /\\@ %a@]" p_dt x p_dt y
  |  Dt.Bor (x, y) -> fprintf ppf "@[%a \\/@.%a@]" p_dt x p_dt y
  |  Dt.Bxor (x, y) -> fprintf ppf "@[%a +@.%a@]" p_dt x p_dt y
  | _ -> assert false
;;

let pout_dt x =
  p_dt std_formatter x;
  fprintf std_formatter "@."
;;

let rec p_bdd ppf = function
  | Bdd.Btrue -> fprintf ppf "true"
  | Bdd.Bfalse -> fprintf ppf "false"
  | Bdd.Bp (_, p, x, y) -> fprintf ppf "@[if@ %s@ (%a, %a)@]" p p_bdd x p_bdd y
  | Bdd.Bnot (_, x) -> fprintf ppf "~%a" p_bdd x 
  | Bdd.Band (_, x, y) ->  fprintf ppf "@[%a@ /\\@ %a@]" p_bdd x p_bdd y
  | Bdd.Bor (_, x, y) -> fprintf ppf "@[%a \\/@.%a@]" p_bdd x p_bdd y
  | Bdd.Bxor (_, x, y) -> fprintf ppf "@[%a +@.%a@]" p_bdd x p_bdd y
  | _ -> assert false
;;

let pout_bdd x =
  p_bdd std_formatter x;
  fprintf std_formatter "@."
;;
