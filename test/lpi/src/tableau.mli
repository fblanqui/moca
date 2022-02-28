(* $Id: tableau.mli,v 1.1 2007-09-21 17:15:33 weis Exp $ *)

(** Implementation of the tableau method for classical propositional logic.
   This should be checked against Smullyan, and the correctness and
   completeness should be provable in Coq. *)

(* Use the classical propositional logic data type definitions. *)
type pattern = Formula.formula Semi_algebra.sequent_set;;
type model_scheme = Formula.atomic_formula Semi_algebra.sequent_set;;

val model_of_pattern : pattern -> model_scheme;;
