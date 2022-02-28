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

(* useful functions on sets *)

open Useful;;

module type S = sig
  include Set.S;;
  val map : (elt -> elt) -> t -> t;;
  val map_filter : (elt -> elt option) -> t -> t;;
  val find : (elt -> bool) -> t -> elt;;
  val find_opt : (elt -> 'a option) -> t -> 'a option;;
  val of_list : elt list -> t;;
  val of_list_map : ('a -> elt) -> 'a list -> t;;
  val fprintf : t fprintf;;
  val union_map : ('a -> t) -> 'a list -> t;;
end

module Make (X : ORD_PRT) = struct

  module M = Set.Make (X);;

  include M;;

(* map on sets *)

let map f xs = M.fold (fun x xs -> M.add (f x) xs) xs M.empty;;

(* Some set of elements y such that f x = Some y for some x in xs.
None otherwise *)

let map_filter f xs =
  let g x (b,xs) =
    match f x with
      | Some y -> true, M.add y xs
      | None -> b, xs
  in match M.fold g xs (false, M.empty) with
    | true, xs -> xs
    | _ -> raise Not_found;;

(* provide Some element satisfying p or None *)

let find_opt p =
  let f x = function None -> p x | o -> o in
    fun xs -> M.fold f xs None;;

(* provide an element satisfying p. raise Not_found otherwise *)

let find p =
  let q x = if p x then Some x else None in
    fun xs -> match find_opt q xs with
      | Some x -> x
      | _ -> raise Not_found;;

(*exception Found of M.elt;;

let find_opt p =
  let f x = if p x then raise (Found x) else () in
    fun xs ->
      try M.iter f xs; raise Not_found
      with Found x -> x;;*)

(* build a set from a list of elements *)

let of_list = List.fold_left (fun xs x -> M.add x xs) M.empty;;
let of_list_map f = List.fold_left (fun xs x -> M.add (f x) xs) M.empty;;

(* print a set into a buffer *)

let fprintf_elt b = Format.fprintf b "%a,\n" X.fprintf;;
let fprintf_set b = M.iter (fprintf_elt b);;
let fprintf b = Format.fprintf b "{%a}" fprintf_set;;

(* union of the sets obtained by mapping f on some list *)

let union_map f =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (M.union (f x) acc) xs
  in aux M.empty;;

end;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
