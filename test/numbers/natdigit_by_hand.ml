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

(* $Id: natdigit_by_hand.ml,v 1.4 2007-11-21 12:49:59 blanqui Exp $ *)

(* Implementation of the system DA in: H. R. Walters and H. Zantema,
   Rewrite Systems for Integer Arithmetic, in Proceedings of the 6th
   Conference on Rewriting Techniques and Applications, 1995, editor
   J. Hsiang, Springer Lecture Notes in Computer Science, volume 914,
   pages 324 - 338.  report version:
   ftp://ftp.cs.ruu.nl/pub/RUU/CS/techreps/CS-1994/1994-43.ps.gz *)

open Natge;;

module Make (R : sig val radix : Natge2.t end) :
sig
  module D : Natge.S;;

  type t = private
    | Empty
    | Snoc of t * D.t (* Snoc = opposite of Cons *)
	(*begin
	  rule Snoc (Empty, D.Mk 0) -> Empty
	end*)
  ;;

  val empty : t;;
  val snoc : t * D.t -> t;;
  val of_nat : Nat.t -> t;;
  val bprintf : Buffer.t -> t -> unit;;
  val succ : t -> t;;
  val add : t -> t -> t;;
  val prod : t -> t -> t;;
  val pred : t -> t;;
  (*val minus : t -> t -> t;;*)
end
=
struct

  let r = Natge2.value R.radix;;

  module D = Modint.Make(R);;

  let digit = Array.init r (fun i -> D.mk i);;

  type t =
    | Empty
    | Snoc of t * D.t (* Snoc = opposite of Cons *)
	(*begin
	  rule Snoc (Empty, D.Mk 0) -> Empty
	end*)
  ;;

  let empty = Empty;;

  let snoc = function
    | Empty, D.Mk 0 -> Empty
    | x, md -> Snoc (x, md)
  ;;

  let rec of_nat_aux x (* >= 0 *) =
    if x < r then snoc (empty, digit.(x))
    else snoc (of_nat_aux (x / r), digit.(x mod r));;

  let of_nat (Nat.Mk x) = of_nat_aux x;;

  let rec bprintf_aux ob = function
    | Empty -> ()
    | Snoc (x, D.Mk d) -> Printf.bprintf ob "%a%i" bprintf_aux x d
  ;;

  let bprintf ob = function
    | Empty -> Printf.bprintf ob "0"
    | x -> bprintf_aux ob x
  ;;
 
  let digit_add x md1 md2 = snoc (x, D.add md1 md2);;

  let rec digit_carry x (D.Mk d1) (D.Mk d2) =
    if d1+d2 < r then x else succ x

  and succ = function
    | Empty -> snoc (empty, digit.(1))
    | Snoc (x, md) -> digit_add (digit_carry x digit.(1) md) digit.(1) md
  ;;

  let rec add u v =
    match u, v with
      | Empty, x
      | x, Empty -> x
      | Snoc (x, md1), Snoc (y, md2) ->
	  digit_add (digit_carry (add x y) md1 md2) md1 md2
  ;;

  let prod_table =
    Array.init r (fun i -> Array.init r (fun j -> of_nat_aux (i * j)));;

  let rec digit_prod (D.Mk d1 as md1) v =
    match v with
      | Empty -> Empty
      | Snoc (x, D.Mk d2) ->
	  add (snoc (digit_prod md1 x, digit.(0))) prod_table.(d1).(d2)
  ;;

  let rec prod u y =
    match u with
      | Empty -> Empty
      | Snoc (x, md) -> add (snoc (prod x y, digit.(0))) (digit_prod md y)
  ;;

  let compare =
    let rec aux acc x y =
      match x, y with
	| Empty, Empty -> acc
	| Empty, Snoc (_, _) -> -1
	| Snoc (_, _), Empty -> 1
	| Snoc (x1, D.Mk d1), Snoc (x2, D.Mk d2) ->
	    let new_acc =
	      match Pervasives.compare d1 d2 with
		| 0 -> acc
		| n -> n
	    in aux new_acc x1 x2
    in aux 0
  ;;

  let rec pred = function
    | Empty -> Empty
    | Snoc (x, D.Mk 0) -> snoc (pred x, digit.(r-1))
    | Snoc (x, D.Mk d) -> snoc (x, digit.(d-1))
  ;;

end;;

module R2 = struct let radix = Natge2.mk 2 end;;

module Natbin = Make (R2);;

module R10 = struct let radix = Natge2.mk 10 end;;

module Decimal = Make (R10);;
