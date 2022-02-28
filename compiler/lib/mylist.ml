(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2012,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* useful functions on lists *)

(* given a list [l] and a number [i] such that [0 <= i < List.length l],
returns the triple [l1, x, l2] such that [x = List.nth l i] and [l = List.rev l1 @ x :: l2] *)

let split =
  let rec aux l1 i l2 =
    match i, l2 with
    | 0, x :: xs -> l1, x, xs
    | n, x :: xs -> aux (x :: l1) (n-1) xs
    | _ -> invalid_arg "split.aux" in
  fun l i ->
    if i < 0 or l = [] then invalid_arg "split" else
    aux [] i l
;;

(* replace i-th element *)

let replace newx =
  let rec aux l1 i l2 =
    match i, l2 with
    | 0, _ :: xs -> List.fold_left (fun l x -> x :: l) (newx :: xs) l1
    | n, x :: xs -> aux (x :: l1) (n - 1) xs
    | _ -> invalid_arg "split.aux" in
  fun l i ->
    if i < 0 then invalid_arg "split" else
    aux [] i l
;;

(* replace i-th element x by f(x) *)

let replace_map f =
  let rec aux l1 i l2 =
    match i, l2 with
    | 0, x :: xs -> List.fold_left (fun l x -> x :: l) (f x :: xs) l1
    | n, x :: xs -> aux (x :: l1) (n - 1) xs
    | _ -> invalid_arg "split.aux" in
  fun l i ->
    if i < 0 then invalid_arg "split" else
    aux [] i l
;;

(* position of an element *)

let position y =
  let rec aux i = function
    | [] -> invalid_arg "position"
    | x :: xs -> if y = x then i else aux (i + 1) xs in
  aux 0
;;

(* print a list *)

let fprintf_list fprintf_elt =
  let rec aux ppf = function
    | [] -> Format.fprintf ppf "[]"
    | x :: xs -> Format.fprintf ppf "%a :: %a" fprintf_elt x aux xs in
  aux
;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
