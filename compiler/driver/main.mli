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

(* $Id: main.mli,v 1.6 2012-06-04 13:01:22 weis Exp $ *)

(** {3 Driving the mocac compiler and its options.} *)

(** A compiler generated interface, just to be able to use the functions in a
    Caml toplevel. *)

exception Usage_error of string
val usage : string -> 'a
module Str : sig type t = string val compare : 'a -> 'a -> int end
module StrSet :
  sig
    type elt = Str.t
    type t = Set.Make(Str).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val add_local_module : StrSet.elt -> unit
val is_local_module : StrSet.elt -> bool
val get_genr_interface : unit -> bool
val set_genr_interface : unit -> unit
val get_genr_implementation : unit -> bool
val set_genr_implementation : unit -> unit
val get_genr_test : unit -> bool
val set_genr_test : unit -> unit
val get_seed : unit -> int
val set_seed : int -> unit
val get_num_tests : unit -> int
val set_num_tests : int -> unit
val get_val_depth : unit -> int
val set_val_depth : int -> unit
val get_testing_file_name : unit -> File.file_name
val set_testing_file_name : File.file_name -> unit
val get_output_module_implementation_name : unit -> File.file_name
val set_output_module_implementation_name : File.file_name -> unit
val get_output_module_interface_name : unit -> File.file_name
val set_output_module_interface_name : File.file_name -> unit
val get_output_module_name : unit -> string
val set_output_module_name : string -> unit
val get_input_file_name : unit -> File.file_name
val set_input_file_name : File.file_name -> unit
val set_implementation_from_file_name : File.file_name -> unit
val set_output_file_name : File.file_name -> unit
val check_signature_item : string -> Parsetree.signature_item -> unit
val check_structure_item : string -> Parsetree.structure_item -> unit
val check_and_genr_ml :
  bool -> Format.formatter -> Parsetree.structure_item -> unit
val check_and_genr_mli :
  bool -> Format.formatter -> Parsetree.structure_item -> unit
val genr_test : int -> Parsetree.structure_item -> Code.structure_item list
val genr_file :
  (Format.formatter -> unit) ->
  (bool -> Format.formatter -> 'a -> unit) ->
  'a list -> File.file_name -> unit
val genr_file_mli : Parsetree.structure_item list -> File.file_name -> unit
val genr_file_ml : Parsetree.structure_item list -> File.file_name -> unit
val genr_test_file : Parsetree.structure_item list -> File.file_name -> unit
val genr : File.file_name -> unit
val umsg : string
val print_moca_version : string -> 'a
val print_moca_short_version : unit -> 'a
val print_moca_full_version : unit -> 'a
val argspec : (Arg.key * Arg.spec * Arg.doc) list
val print_usage : unit -> unit
val main : unit -> 'a
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
