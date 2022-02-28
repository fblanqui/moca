(* $Id: formula.mli,v 1.1 2007-09-24 16:19:20 weis Exp $ *)

type 'a set = Empty | Cons of 'a * 'a set;;
(** General polymorphic sets. *)

type identifier = string
  (** An identifier is just a name. *)
;;

type propositional_letter = Letter of identifier
  (** A propositional letter is a name of a proposition. *)
;;

type atomic_formula =
   | T
   | F
   | P of propositional_letter
     (** An atomic formula is either a propositional constant
         truth value or a propositional letter. *)
;;

type formula =
   | Atom of atomic_formula
   | Not of formula
   | Imply of formula * formula
   | Or of formula * formula
   | And of formula * formula
   | Equiv of formula * formula
;;
