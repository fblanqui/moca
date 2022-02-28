
(* data structure keeping track of normalized subterms *)

open Var;;
open Term;;

type t =
  | NVar of var
  | NApp of Symb.symb * nterm list

and nterm = {
  term : t;
  norm : bool;
};;

(* convert a term into an nterm *)

let rec nterm_of_term = function
  | Term.App (f,ts) -> { term = NApp (f, List.map nterm_of_term ts); norm = false }
  | Term.Var x -> { term = NVar x; norm = true };;

(* convert an nterm into a term *)

let rec term_of_nterm { term = t } =
  match t with
    | NApp (f,nts) -> App (f, List.map term_of_nterm nts)
    | NVar x -> Var x;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
