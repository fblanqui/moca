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

(* builtin equations in moca *)

type side = Left | Right;;

open Symb;;

type axiom =
   | Com of symbol
   | Ass of symbol
   | Nil of symbol * symbol
   | Idem of symbol
   | Neu of side * symbol * symbol
   | Abs of side * symbol * symbol
   | Inv of side * symbol * symbol * symbol
   | Dis of side * symbol * symbol
   | InvDis of side * symbol * symbol
   | UDis of symbol * symbol * symbol
   | Invol of symbol
   | UNil of symbol * symbol
   | UIdem of symbol
   | InvNeu of symbol * symbol * symbol
;;

type theory = axiom list;;

let com c = [Com c];;
let ass c = [Ass c];;
let nil c a = [Nil (c,a)];;
let idem c = [Idem c];;
let lneu c e = [Neu (Left,c,e)];;
let rneu c e = [Neu (Right,c,e)];;
let neu c e = lneu c e @ rneu c e;;
let labs c e = [Abs (Left,c,e)];;
let rabs c e = [Abs (Right,c,e)];;
let abs c e = labs c e @ rabs c e;;
let linv c i e = [Inv (Left,c,i,e)];;
let rinv c i e = [Inv (Right,c,i,e)];;
let inv c i e = linv c i e @ rinv c i e;;
let ldis c d = [Dis (Left,c,d)];;
let rdis c d = [Dis (Right,c,d)];;
let dis c d = ldis c d @ rdis c d;;
let linvdis c i = [InvDis (Left,c,i)];;
let rinvdis c i = [InvDis (Right,c,i)];;
let invdis c i = linvdis c i @ rinvdis c i;;
let udis i c d = [UDis (i,c,d)];;
let invol c = [Invol c];;
let unil c a = [UNil (c,a)];;
let uidem c = [UIdem c];;
let invneu c e a = [InvNeu (c,e,a)];;

open Term;;

let app0 c = App (c, []);;
let app1 c x = App (c, [x]);;
let app2 c x y = App (c, [x;y]);;

let term_pair_of_axiom =
  let x = Var 0 and y = Var 1 and z = Var 2 in
  function
    | Com c -> app2 c x y, app2 c y x
    | Ass c -> app2 c (app2 c x y) z, app2 c x (app2 c y z)
    | Nil (c,a) -> app2 c x x, app0 a
    | Idem c -> app2 c x x, x
    | Neu (Left, c, e) -> let e = app0 e in app2 c e x, x
    | Neu (Right, c, e) -> let e = app0 e in app2 c x e, x
    | Abs (Left, c, a) -> let a = app0 a in app2 c a x, a
    | Abs (Right, c, a) -> let a = app0 a in app2 c x a, a
    | Inv (Left, c, i, e) -> let e = app0 e in app2 c (app1 i x) x, e
    | Inv (Right, c, i, e) -> let e = app0 e in app2 c x (app1 i x), e
    | Dis (Left, c, d) ->
        app2 c (app2 d x y) z, app2 d (app2 c x z) (app2 c y z)
    | Dis (Right, c, d) ->
        app2 c x (app2 d y z), app2 d (app2 c x y) (app2 c x z)
    | InvDis (Left, c, i) -> app2 c (app1 i x) y, app1 i (app2 c x y)
    | InvDis (Right, c, i) -> app2 c x (app1 i y), app1 i (app2 c x y)
    | UDis (i, c, d) -> app1 i (app2 c x y), app2 d (app1 i y) (app1 i x)
    | Invol c -> app1 c (app1 c x), x
    | UNil (c, a) -> let a = app0 a in app1 c (app1 c x), a
    | UIdem c -> app1 c (app1 c x), app1 c x
    | InvNeu (i, e, a) -> let a = app0 a and e = app0 e in app1 i e, a;;

open Equation;;

let eqnset_of_theory =
  let rec aux acc = function
    | [] -> EqnSet.empty
    | x :: xs -> aux (EqnSet.add (Equation.mk (term_pair_of_axiom x)) acc) xs
  in aux EqnSet.empty
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
