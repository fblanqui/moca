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

(* $Id: tring_list.ml,v 1.1 2012-02-27 11:37:29 bonichon Exp $ *)

(* Testing a ring structure with variables. *)

(* To test interactively, use:

$ ./ocaml_test ring_list
+ ocamlc gentest.mli
+ ocamlc gentest.ml
+ ../mocac ring_list.mlm
+ ocamlc ring_list.mli
+ ocamlc ring_list.ml
# #use "tring_list.ml";;
*)

open Gentest;;

open Ring_list;;

testing "Ring_list";;

testl
 (111)
  (try
     (let x0 =
        mul
        [ (add [ (gen (62)); (gen (63)) ]);
          (add [ (gen (0)); (gen (61)) ]);
        ]  in
    let x1 =
      mul
        [ (add [ (gen (55)); (gen (63)) ]);
          (mul [ (gen (21)); (gen (54)) ]); (gen (65)) ] in
    let x2 = x0 in
    mul [ x0; x1; (opp x0); x2 ] = opp (mul [ x0; x1; x0; x2 ])) with
   | Failure "Division by Absorbent" -> true)
;;

(* seed: 954173690 *)
testl
  (112)
  (try
   (let x0 =
      mul
        [ (add [ (gen (55)); (gen (62)); (gen (63)) ]);
          (add [ (gen (31)); (gen (0)); (gen (61)) ]);
          (add [ (gen (13)); (gen (23)); (gen (29)) ]) ] in
    let x1 =
      mul
        [ (add [ (gen (55)); (gen (62)); (gen (63)) ]);
          (mul [ (gen (21)); (gen (72)); (gen (54)) ]); (gen (65)) ] in
    let x2 =
      mul
        [ (add [ (gen (55)); (gen (62)); (gen (63)) ]);
          (add [ (gen (31)); (gen (0)); (gen (61)) ]);
          (add [ (gen (13)); (gen (23)); (gen (29)) ]) ] in
    mul [ x0; x1; (opp x0); x2 ] = opp (mul [ x0; x1; x0; x2 ])) with
   | Failure "Division by Absorbent" -> true)
;;
