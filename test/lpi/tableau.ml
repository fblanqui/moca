(* $Id: tableau.ml,v 1.2 2008-02-13 23:23:52 weis Exp $ *)

(* Use the classical propositional logic data type definitions. *)
type pattern = Formula.formula Semi_algebra.sequent_set;;
type model_scheme = Formula.atomic_formula Semi_algebra.sequent_set;;

open Formula;;
open Semi_algebra;;

(* Implementation of the tableau method for classical propositional logic.
   This should be checked against Smullyan, and the correctness and
   completeness should be provable in Coq. *)

let rec model_of_pattern p =
  match p with
  | Trivial -> trivial
  | Positive f ->
    begin match f with
    | Atom T -> trivial
    | Atom F -> closed
    | Atom p -> positive p
    | Not p -> model_of_pattern (negative p)
    | And (p, q) ->
      graft (model_of_pattern (positive p), model_of_pattern (positive q))
    | Or (p, q) ->
      union (model_of_pattern (positive p), model_of_pattern (positive q))
    | Imply (p, q) ->
      union (model_of_pattern (negative p), model_of_pattern (positive q))
    | Equiv (p, q) ->
      union
       (graft (model_of_pattern (negative p),
               model_of_pattern (negative q)),
        graft (model_of_pattern (positive p),
               model_of_pattern (positive q)))
    end
  | Negative f ->
    begin match f with
    | Atom T -> closed
    | Atom F -> trivial
    | Atom (p) -> negative (p)
    | Not p -> model_of_pattern (positive p)
    | And (p, q) ->
      union (model_of_pattern (negative p), model_of_pattern (negative q))
    | Or (p, q) ->
      graft (model_of_pattern (negative p), model_of_pattern (negative q))
    | Imply (p, q) ->
      graft (model_of_pattern (positive p), model_of_pattern (negative q))
    | Equiv (p, q) ->
      union
       (graft (model_of_pattern (negative p),
               model_of_pattern (positive q)),
        graft (model_of_pattern (positive p),
               model_of_pattern (negative q)))
   end
  | Union (p1, p2) -> union (model_of_pattern p1, model_of_pattern p2)
  | Graft (p1, p2) -> graft (model_of_pattern p1, model_of_pattern p2)
  | Closed -> closed
;;
