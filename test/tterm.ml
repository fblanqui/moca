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

(* $Id: tterm.ml,v 1.4 2012-06-04 13:01:22 weis Exp $ *)

(* Testing terms with no rules or equations. *)

open Gentest;;

open Term;;

testing "Term"
;;

let var0 = var 0;;
let var1 = var 1;;
let var2 = var 2;;
let lvar0 = [var0];;
let lvar1 = [var1];;
let lvar2 = [var2];;


(* var *)
testl 00 (var0 = var0);
testl 01 (var0 <> var1);
testl 02 (match var0 with
          | Var _ -> true
          | _ -> false);

(* app *)
testl 10 (app (var0, lvar1) <> app (var1, lvar0));
testl 11 (app (var0, [var1; var2]) <> app (var0, [var2; var1]));
testl 12 (app (var0, [app (var1, lvar2)]) <> app (var0, [var1; var2]));
testl 13 (app (var0, [app (var1, lvar2)]) <> app (app (var0, lvar1), lvar2));
testl 14 (app (var0, [var0]) <> var0);
testl 15 (app (var0, [var1; var1]) <> app (var0, [var1]));
testl 16
  (match app (var0,
              [app (app (var0, lvar0),
                    [app (app (var0, lvar0),
                       [var0])])]) with
   | App (Var _,
          [App (App (Var _, [Var _]),
                [App (App (Var _, [Var _]),
                        [Var _])])]) -> true
   | _ -> false)
;;
