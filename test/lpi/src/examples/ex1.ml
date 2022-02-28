(* Classical tautology test. *)

(* $Id: ex1.ml,v 1.1 2007-09-21 17:15:33 weis Exp $ *)

(*
#load "lpi.cma";;
#use "examples/ex1.ml";;
*)

open Formula;;
open Semi_algebra;;
open Tableau;;

let tautology t = model_of_pattern (negative t) = closed;;

let counter_example t =
  match model_of_pattern (negative t) with
  | Closed -> failwith "formula is a tautology"
  | Trivial -> failwith "formula is an antilogy"
  | Union (_, _)
  | Graft (_, _)
  | Positive _
  | Negative _ as mods -> mods;;

let rec print_atomic_formula = function
  | T -> print_string "true"
  | F -> print_string "false"
  | P (Letter v) -> print_string v;;

let rec print_formula = function
  | Atom a -> print_atomic_formula a
  | Not f -> print_string "not "; print_formula f
  | Imply (f1, f2) ->
    print_formula f1;
    print_string " => ";
    print_formula f2;
  | Or (f1, f2) ->
    print_formula f1;
    print_string " || ";
    print_formula f2;
  | And (f1, f2) ->
    print_formula f1;
    print_string " && ";
    print_formula f2;
  | Equiv (f1, f2) ->
    print_formula f1;
    print_string " <=> ";
    print_formula f2;;

let rec print_sequent_set print_formula = function
  | Trivial -> print_string "any model"
  | Closed ->  print_string "no model"
  | Positive f -> print_string "|= "; print_formula f
  | Negative f -> print_string "|/= "; print_formula f
  | Union (s1, s2) ->
    print_sequent_set print_formula s1;
    print_sequent_set print_formula s2;
  | Graft (s1, s2) ->
    print_sequent_set print_formula s1;
    print_sequent_set print_formula s2;
;;

let print_pattern = print_sequent_set print_formula;;
let print_model_scheme = print_sequent_set print_atomic_formula;;

let p = Atom (P (Letter "p"));;
let q = Atom (P (Letter "q"));;
let r = Atom (P (Letter "r"));;

let ( => ) = fun p q -> Imply (p, q);;

(* I *)
let i = p => p;;
(* K *)
let k = p => (q => p);;
(* T : fun x y z -> y (x z) *)
let t = (p => q) => ((q => r) => (p => r));;

(* S : fun x y z -> (x z) (y z) *)
let s = (p => (q => r)) => ((p => q) => (p => r));;

(* k' is weaker than k. *)
let k' = ((p => q) => r) => ((p => q) => (p => r));;

let test0 =
  tautology i && tautology k && tautology t && tautology s &&
  tautology k';;

print_model_scheme (counter_example (p => q));;

let main () =
  if not !Sys.interactive then
  if test0 then prerr_endline "Tests succeeded!" else
    (prerr_endline "Tests failed!"; exit 2);;

main ();;
