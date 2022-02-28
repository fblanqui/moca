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

open Group_list
;;

open Gentest
;;

testing "Group_list"
;;

(* Automatic tests that once failed *)
testi 0
(let x = mult [mult [gen 8; gen 99; gen 18]; gen 67; opp (gen 72)] in
 let y = opp (mult [gen 9; gen 67; gen 96]) in
 let z = gen 93 in
 let w = gen 19 in
 mult [mult [x; y; z]; w] = mult [x; y; z; w])
;;

testi 1
(let x = gen 80 in
 let y =
   mult
     [opp (gen 95); mult [gen 90; gen 124; gen 39]; mult
      [gen 68; gen 117; gen 73]] in
 let z = opp (gen 117) in
 let w = mult [gen 90; mult [gen 90; gen 124; gen 39]; opp (gen 71)] in
 let x1 =
   mult
     [mult [gen 46; gen 97; gen 84]; mult [gen 47; gen 95; gen 41]; mult
      [gen 119; gen 98; gen 73]] in
 mult [x; mult [y; z]; mult [w; x1]] = mult [x; y; z; w; x1])
;;

testi 2
(let x = opp (gen 23) in
 let y = mult [opp (gen 63); gen 23; mult [gen 64; gen 68; gen 74]] in
 let z =
   mult
     [mult [gen 12; gen 26; gen 3]; mult [gen 0; gen 23; gen 65]; mult
      [gen 64; gen 45; unit]] in
 mult [mult [x; y]; z] = mult [x; y; z])
;;
(*
testi 3
(let x = gen 64 in
 let y = mult [gen 32; mult [gen 27; gen 57; gen 1]; opp unit] in
 let z =
   mult [gen 36; mult [gen 82; gen 3; gen 55]; mult [gen 61; unit; gen 39]] in
 let w =
   mult
     [mult [gen 58; gen 64; gen 111]; mult [gen 82; gen 3; gen 55]; mult
      [gen 61; unit; gen 39]] in
 let x1 = opp (mult [gen 0; gen 64; gen 44]) in
 mult [x; mult [y; z]; mult [w; x1]] = mult [x; y; z; w; x1])
;;*)
