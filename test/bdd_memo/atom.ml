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

(* $Id: atom.ml,v 1.2 2009-04-22 09:49:32 weis Exp $ *)

let debug = ref false;;

type atom = int
;;

external from : atom -> int = "%identity"
;;

let symbol_table = (Hashtbl.create 10 : (string, atom) Hashtbl.t)
;;

let atom_table = (Hashtbl.create 10 : (atom, string) Hashtbl.t)
;;

let next_atom =
  let atom_counter = ref 0 in
  (fun () ->
   incr atom_counter;
   !atom_counter)
;;

let create_atom s =
  if not (Hashtbl.mem symbol_table s) then begin
    if !debug then prerr_endline (Printf.sprintf "Creating atom %s" s);
    let atom = next_atom () in
    Hashtbl.add symbol_table s atom;
    Hashtbl.add atom_table atom s;
  end
;;

let get_atom s =
  let a = Hashtbl.find symbol_table s in
  if !debug then prerr_endline (Printf.sprintf "get_atom %s: %d" s (from a));
  a
;;

(* Implementing the interface. *)
let make s =
  create_atom s;
  get_atom s
;;

let name a =
  try Hashtbl.find atom_table a with
  | Not_found ->
    failwith
      (Printf.sprintf
         "Atom.name: no atom number \
          %d has ever been defined."
         (from a))
;;

let compare = ( - )
;;

external eq : atom -> atom -> bool = "%eq"
;;

