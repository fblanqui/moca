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

(* $Id: memo_htbl.ml,v 1.2 2009-04-22 09:49:32 weis Exp $ *)

type ('a, 'b) t = {
  hash : 'a -> int;
  eq : 'a -> 'a -> bool;
  mutable length : int;
  mutable values : ('a * 'b) list array;
}
;;

let create h eq len =
  if len <= 0 ||
     len >= Sys.max_array_length
  then invalid_arg (Printf.sprintf "Memo_tbl.create %d" len) else

  let hash x =
    let h_code = h x in
    if h_code < 0 then
      failwith (Printf.sprintf "Memo_tbl: bad hashing value %d" h_code) else
    h_code in

  { hash = hash;
    eq = eq;
    length = len;
    values = Array.make len []; }
;;

let add t x y =
  let h = t.hash x mod t.length in
  t.values.(h) <- (x, y) :: t.values.(h)
;;

let find t x =
  let h = t.hash x mod t.length in
  let bucket = t.values.(h) in
  let rec find_in_bucket x = function
    | [] -> raise Not_found
    | (x0, y0) :: rest -> if t.eq x x0 then y0 else find_in_bucket x rest in
  find_in_bucket x bucket
;;

let mem t x =
  let h = t.hash x mod t.length in
  let bucket = t.values.(h) in
  let rec find_in_bucket x = function
    | [] -> false
    | (x0, y0) :: rest -> t.eq x x0 || find_in_bucket x rest in
  find_in_bucket x bucket
;;
