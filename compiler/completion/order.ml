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

(* $Id: order.ml,v 1.24 2012-04-02 09:27:25 weis Exp $ *)

(* Result type for comparison functions. *)

type ord =
   | Greater
   | Equal
   | Smaller
   | Uncomparable
;;

type 'a cmp = 'a -> 'a -> ord;;

(* partition a list of pairs into those whose lhs is greater, equal, etc.
than the rhs *)

let partition cmp =
  let rec aux g e s u = function
    | [] -> g, e, s, u
    | ((l,r) as x) :: xs ->
        match cmp l r with
          | Greater -> aux (x::g) e s u xs
          | Equal -> aux g (x::e) s u xs
          | Smaller -> aux g e (x::s) u xs
          | Uncomparable -> aux g e s (x::u) xs
  in aux [] [] [] []
;;

(* basic functions on ord *)

let opp = function
  | Greater -> Smaller
  | Smaller -> Greater
  | (Equal | Uncomparable) as x -> x
;;

let ge = function
  | Greater | Equal -> true
  | Smaller | Uncomparable -> false
;;

let le = function
  | Smaller | Equal -> true
  | Greater | Uncomparable -> false
;;

let comp o1 o2 =
  match o1, o2 with
    | Smaller, Smaller | Smaller, Equal | Equal, Smaller
      -> Smaller
    | Equal, Equal
      -> Equal
    | Greater, Greater | Greater, Equal | Equal, Greater
      -> Greater
    | Greater, Smaller | Smaller, Greater | Uncomparable, Uncomparable
    | Uncomparable, Smaller | Uncomparable, Equal | Uncomparable, Greater
    | Smaller, Uncomparable | Equal, Uncomparable | Greater, Uncomparable
      -> Uncomparable
;;

let int_of_ord = function
  | Greater -> 1
  | Equal -> 0
  | Smaller -> -1
  | Uncomparable -> raise (Invalid_argument "inf_of_ord");;

let gt_if = function
  | true -> Greater
  | false -> Uncomparable
;;

let lt_if = function
  | true -> Smaller
  | false -> Uncomparable
;;

let eq_if = function
  | true -> Equal
  | false -> Uncomparable
;;

(* lexicographic extension of an ordering *)

let lex cmp =
  let rec aux xs ys =
    match xs, ys with
      | [], [] -> Equal
      | x :: xs2, y :: ys2 ->
          (match cmp x y with
             | Equal -> aux xs2 ys2
             | (Smaller | Greater | Uncomparable) as n -> n)
      | _ -> Uncomparable
  in aux
;;

(* multiset extension of an ordering *)
(* See Baader and Nipkow's Term Rewriting and All That, p.27 *)

let mul cmp =
  let rec rem xs y =
    match xs with
    | [] -> xs
    | x :: xs1 ->
        match cmp x y with
        | Equal -> xs1
        | Greater | Smaller | Uncomparable -> x :: rem xs1 y in
  let rec mdiff xs ys =
    match ys with
    | [] -> xs
    | y :: ys1 ->
        mdiff (rem xs y) ys1 in
  let mul_is_greater e1 e2 =
    List.for_all
      (fun n -> List.exists (fun m -> cmp m n = Greater) e1) e2 in
  let aux m n =
    let n_m = mdiff n m
    and m_n = mdiff m n in
    match n_m, m_n with
    | [], [] -> Equal
    | _, _ ->
        if mul_is_greater m_n n_m
        then Greater
        else
          if mul_is_greater n_m m_n then Smaller
          else Uncomparable
  in aux
;;

(* lexicographic path ordering *)

open Symb;;
open Term;;
open Parsetree;;

exception IncompletePrecedence of symbol * symbol;;

let rec has_one_greater cmp ts t  =
  match ts with
  | [] -> false
  | x :: xs ->
      (cmp x t = Greater) || (cmp x t = Equal) ||
      (has_one_greater cmp xs t)
;;

let rec greater_than_all cmp t = function
  | [] -> true
  | x :: xs ->
      if cmp t x = Greater then greater_than_all cmp t xs
      else false
;;

(********** Unused!

let lpo_fail prec =
  let rec lpo_rec t u =
    match t, u with
      | (App (f, ts) as t), (App (g, us) as u) ->
          if has_one_greater lpo_rec ts u then Greater
          else if has_one_greater lpo_rec us t then Smaller
          else
            begin
             match prec f g with
               | Equal ->
                   begin
                    match lex lpo_rec ts us with
                   | Equal -> Equal
                   | Greater ->
                       gt_if (greater_than_all lpo_rec t us)
                   | Smaller ->
                       lt_if (greater_than_all lpo_rec u ts)
                   | Uncomparable -> Uncomparable
                   end
               | Greater ->
           gt_if (greater_than_all lpo_rec t us)
               | Smaller ->
           lt_if (greater_than_all lpo_rec u ts)
               | Uncomparable -> raise (IncompletePrecedence (f,g))
            end
      | Var x, Var y -> eq_if (x = y)
      | App _ as t, Var x -> gt_if (occurs x t)
      | Var x, (App _ as t) -> lt_if (occurs x t)
  in lpo_rec
;;

let lpo prec t u =
  try lpo_fail prec t u
  with IncompletePrecedence _ -> Uncomparable;;

(* Multiset Path Ordering *)

let mpo_fail prec =
  let rec mpo_rec t u =
    match t, u with
      | (App (f, ts) as t), (App (g, us) as u) ->
          if has_one_greater mpo_rec ts u then Greater
          else if has_one_greater mpo_rec us t then Smaller
          else
            (match prec f g with
               | Equal -> mul mpo_rec ts us
               | Greater ->
           gt_if (greater_than_all mpo_rec t us)
               | Smaller ->
           lt_if (greater_than_all mpo_rec u ts)
               | Uncomparable -> raise (IncompletePrecedence (f,g)))
      | Var x, Var y -> eq_if (x=y)
      | App _ as t, Var x -> gt_if (occurs x t)
      | Var x, (App _ as t) -> lt_if (occurs x t)
  in mpo_rec
;;

let mpo cmp t u =
  try mpo_fail cmp t u
  with IncompletePrecedence _ -> Uncomparable;;

**************)

(* Recursive Path Ordering *)

exception IncompatibleStatus of symbol * symbol;;

let get status f =
  if SymbolMap.mem f status then Multiset else Lexicographic
;;

let rpo_fail status prec =
  let rec rpo_rec t u =
    match t, u with
    | Var x , Var y -> eq_if (x = y)
    | App _ as t, Var y -> gt_if (occurs y t)
    | Var x, (App _ as u) -> lt_if (occurs x u)
    | (App (f, ts) as t), (App (g, us) as u) ->
       if has_one_greater rpo_rec ts u then Greater
       else if has_one_greater rpo_rec us t then Smaller
       else
         begin
         match prec f g with
         | Uncomparable -> raise (IncompletePrecedence (f, g))
         | Greater ->
             gt_if (greater_than_all rpo_rec t us)
         | Smaller ->
             lt_if (greater_than_all rpo_rec u ts)
         | Equal ->
             begin
             match get status f, get status g with
             | Multiset, Multiset -> mul rpo_rec ts us
             | Lexicographic, Lexicographic ->
                  begin
                   match lex rpo_rec ts us with
                   | Equal -> Equal
                   | Greater ->
                       gt_if (greater_than_all rpo_rec t us)
                   | Smaller ->
                       lt_if (greater_than_all rpo_rec u ts)
                   | Uncomparable -> Uncomparable
                   end
             | Multiset, Lexicographic | Lexicographic, Multiset ->
         raise (IncompatibleStatus (f, g))
             end
          end
  in rpo_rec
;;

let rpo status prec t u =
  try rpo_fail status prec t u
  with IncompletePrecedence _ -> Uncomparable
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
