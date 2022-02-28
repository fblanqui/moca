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

open Order;;
open Term;;
open Symb;;

module SymbolPair = struct
  type t = symbol * symbol
  let compare = Pervasives.compare
end
;;

module Prec = Map.Make (SymbolPair);;

type prec = ord Prec.t;;

let compare prec f g =
  if f = g then Equal else
    try Prec.find (f, g) prec
    with Not_found ->
      try opp (Prec.find (g, f) prec)
      with Not_found -> Uncomparable
;;

let empty = Prec.empty;;

let add_lt_if b (f, g) m = if b then Prec.add (f, g) Smaller m else m;;

let add_lt prec f g =
  match compare prec f g with
  | Smaller -> prec
  | Uncomparable ->
    let func (x, y) o m =
      match o with
      | Smaller ->
        add_lt_if (x = g) (f, y) (add_lt_if (y = f) (x, g) m)
      | Equal ->
        let m =
          if x = f then Prec.add (y, g) Smaller m
          else add_lt_if (x = g) (f, y) m
        in
        if y = f then Prec.add (x, g) Smaller m
        else add_lt_if (y = g) (f, x) m
      | Greater
      | Uncomparable -> assert false
    in Prec.fold func prec (Prec.add (f, g) Smaller prec)
  | Greater
  | Equal -> failwith "add_lt"
;;

let add_sym (f, g) prec =
  Prec.add (f, g) Equal (Prec.add (g, f) Equal prec)
;;

let add_sym_if b (x, y) m = if b then add_sym (x, y) m else m;;

let add_eq prec f g =
  match compare prec f g with
  | Equal -> prec
  | Uncomparable ->
      let func (x, y) o m =
        match o with
        | Smaller ->
          let m =
            if x = f then Prec.add (g, y) Smaller m
            else add_lt_if (x = g) (f, y) m
          in
          if y = f then Prec.add (x, g) Smaller m
          else add_lt_if (y = g) (x, f) m
        | Equal ->
          let m =
            if x = f then add_sym (y, g) m
            else add_sym_if (x = g) (f, y) m
          in
          if y=f then add_sym (x, g) m
          else add_sym_if (y = g) (f, x) m
       | Greater
       | Uncomparable -> assert false
      in Prec.fold func prec (add_sym (f, g) prec)
  | Greater
  | Smaller -> failwith "add_eq"
;;

let add_gt prec f g = add_lt prec g f;;

(* [f1; ..; fn] gives f1 < .. < fn *)

let rec prec_of_list = function
  | f :: ((g :: _) as fs) -> add_lt (prec_of_list fs) f g
  | _ -> empty
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
