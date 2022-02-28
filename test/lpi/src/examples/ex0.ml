(* $Id: ex0.ml,v 1.1 2007-09-21 17:15:33 weis Exp $ *)

(*
open Formula;;
open Tableau;;

let bird = Variable "bird";;
let fly = Variable "fly";;
let penguin = Variable "penguin";;

let a0 = decomposition (True T);;

a0 = Empty;;

let a1 = decomposition (True F);;

a1 = Cons (Cons (True F, Empty), Empty);;

pattern (True F) = no_models;;

let a2 = decomposition (True bird);;

a2 = Cons (Cons (True (Variable "bird"), Empty), Empty);;

let a3 = decomposition (False bird);;

a3 = Cons (Cons (False (Variable "bird"), Empty), Empty);;

let a4 = decomposition (True (And (bird, fly)));;

a4 =
 Cons (Cons (True (Variable "bird"),
             Cons (True (Variable "fly"),
                   Empty)),
       Empty);;

let a5 = decomposition (True (Imply (bird, fly)));;

a5 =
  Cons (Cons (True (Variable "fly"), Empty),
   Cons (Cons (False (Variable "bird"), Empty), Empty));;

let a6 = decomposition (True (Or (bird, fly)));;

a6 = Cons (Cons (True (Variable "bird"), Empty),
   Cons (Cons (True (Variable "fly"), Empty), Empty));;

let a7 = decomposition (True (Or (bird, Not fly)));;

(a7 : signed_formula set set) =
  Cons (Cons (True (Variable "bird"), Empty),
   Cons (Cons (False (Variable "fly"), Empty), Empty));;

let a8 = pattern (True (Imply (bird, fly)));;

(a8 : signed_formula set set) =
  Cons (Cons (True (Variable "fly"), Empty),
   Cons (Cons (False (Variable "bird"), Empty), Empty));;
*)
