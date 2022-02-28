open Skilam;;
open Lam2ski;;
open Type;;
(*open Ski2lam;;*)
open Format;;


let rec p_ls ppf = function
  | Labst (s, t) ->
      fprintf ppf "@[\\%s.@,%a@]" s p_ls t
  | Lvar x | Ssvar x -> fprintf ppf "%s" x
  | Ss -> fprintf ppf "S"
  | Sk -> fprintf ppf "K"
  | Si -> fprintf ppf "I"
  | Lapp (x, y) | Ssapp (x,y) ->
      fprintf ppf "@[(%a@ %a)@]" p_ls x p_ls y
  | Ski x -> fprintf ppf "@[%a@]" p_ski x
  | Toski x -> fprintf ppf "@[%a@]" p_ls x
  | Tolam x -> fprintf ppf "@[%a@]" p_ski x

and p_lam ppf = function
  | Var s -> fprintf ppf "%s" s
  | Abst (s, t) -> fprintf ppf "@[\\%s.%a@]" s p_lam t
  | App (x, y) ->
      fprintf ppf "@[(%a %a)@]" p_lam x p_lam y


and p_ski ppf = function
  | Svar s -> fprintf ppf "%s" s
  | S -> fprintf ppf "S"
  | K -> fprintf ppf "K"
  | I -> fprintf ppf "I"
  | Sapp (x, y) ->
     fprintf ppf "@[%a@ %a@]" p_ski x p_ski y
;;

let rec p_ty ppf = function
  | TyId x -> fprintf ppf "%s" x
  | TyArrow (x, y) -> fprintf ppf "@[(%a)@,->@,%a@]" p_ty x p_ty y
;;

let pout_ls x =
  p_ls std_formatter x;
  fprintf std_formatter "@."

and pout_ski x =
  p_ski std_formatter x;
  fprintf std_formatter "@."
    
and pout_lam x =
  p_lam std_formatter x;
  fprintf std_formatter "@."

and pout_ty x =
  p_ty std_formatter x;
  fprintf std_formatter "@."

;;

