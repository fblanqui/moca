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

let bool_of_opt = function
  | None -> false
  | _ -> true;;

module OrdStr = struct
  type t = string;;
  let compare = Pervasives.compare;;
end;;

module StrMap = Map.Make (OrdStr);;
module StrSet = Set.Make (OrdStr);;

module OrdInt = struct
  type t = int;;
  let compare = Pervasives.compare;;
end;;

module IntMap = Map.Make (OrdInt);;
module IntSet = Set.Make (OrdInt);;

type 'a fprintf = Format.formatter -> 'a -> unit;;

module type ORD_PRT = sig
  type t;;
  val compare : t -> t -> int;;
  val fprintf : t fprintf;;
end;;

(* Not used in Moca.
let value = function
  | Some x -> x
  | None -> raise (Invalid_argument "Useful.value");;

let fprintf_bool ppf = function
  | true -> Format.fprintf ppf "true"
  | false -> Format.fprintf ppf "false";;

let fprintf_int ppf = Format.fprintf ppf "%i";;

let new_line p ppf x = Format.fprintf ppf "%a\n" p x;;

let fprintf_pair sep func ppf (x,y) =
  Format.fprintf ppf "%a%s%a" func x sep func y;;
*)
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
