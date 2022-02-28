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

(* {4 Basic functions on terms } *)

open Term;;

(* fold among subterms *)

let fold add =
  let rec aux acc = function
    | [] -> acc
    | App (_,ts) as t :: us -> aux (add t acc) (ts @ us)
    | Var _ as t :: us -> aux (add t acc) us
  in fun t a -> aux a [t];;

(* find a subterm satisfying some predicate. raise Not_found otherwise *)

let find pred =
  let rec aux = function
    | [] -> None
    | t :: us ->
        if pred t then Some t else
          (match t with
             | App (_,ts) -> aux (ts @ us)
             | Var _ -> aux us)
  in fun t ->
    match aux [t] with
      | Some t -> t
      | _ -> raise Not_found;;

(* find Some subterm satisfying some predicate or None *)

let find_opt pred =
  let add t = function
    | None -> pred t
    | o -> o
  in fun t -> fold add t None;;

(* test whether there exits a subterm satisfying some predicate *)

let exists pred t =
  try let _ = find pred t in true with Not_found -> false;;

(* positions in a term *)

type position = int list;;

(* subterm at position p *)

let rec subterm_pos t p =
  match t, p with
    | t, [] -> t
    | App (_,ts), i :: q -> subterm_pos (List.nth ts i) q
    | Var _, _ -> raise (Invalid_argument "subterm_pos");;

(* replace by u the subterm of t at position p *)

let replace t p u =
  let rec aux p t =
    match p, t with
      | [], _ -> u
      | i :: q, App (f, ts) -> App (f, Mylist.replace_map (aux q) ts i)
      | _, Var _ -> raise (Invalid_argument "replace")
  in aux p t;;

(* functions taking into account the position *)

let add_subterms p ts us =
  fst (List.fold_left (fun (us,i) ti -> (ti,i::p)::us,i+1) (us,0) ts);;

(* fold among subterms *)

let foldi add =
  let add t p = add t (List.rev p) in
  let rec aux acc = function
    | [] -> acc
    | (App (_, ts) as t, p) :: us -> aux (add t p acc) (add_subterms p ts us)
    | (Var _ as t, p) :: us -> aux (add t p acc) us
  in fun t a -> aux a [t,[]];;

(* find a subterm satisfying some predicate. raise Not_found otherwise *)

let findi pred =
  let rec aux = function
    | [] -> None
    | (t,p) :: us ->
        if pred t p then Some (t,p) else
          (match t with
             | App (_,ts) -> aux (add_subterms p ts us)
             | Var _ -> aux us)
  in fun t ->
    match aux [t,[]] with
      | Some x -> x
      | _ -> raise Not_found;;

(* find Some subterm satisfying some predicate or None *)

let find_opti pred =
  let add t p = function
    | None ->
        (match pred t p with
           | Some x -> Some (x,p)
           | _ -> None)
    | o -> o
  in fun t -> foldi add t None;;

(* test whether there exits a subterm satisfying some predicate *)

let existsi pred t =
  try let _ = findi pred t in true with Not_found -> false;;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
