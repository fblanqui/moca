open Fol;;
open Format;;

let rec p_fol ppf = function
  | Btrue -> fprintf ppf "true"
  | Bfalse -> fprintf ppf "false"
  | Prop (_, p, ts) -> fprintf ppf "@[%s%a@]" p p_terms ts
  | Bnot x -> fprintf ppf "~%a" p_fol x
  | Band (x, y) ->  fprintf ppf "@[%a@ /\\@ %a@]" p_fol x p_fol y
  | Bimplies (x, y) -> fprintf ppf "[@%a@,=>@,%a@]" p_fol x p_fol y
  | Bor (x, y) -> fprintf ppf "@[<v 0>%a \\/@ %a@]" p_fol x p_fol y
  | Ball (x, f) -> fprintf ppf "@[\\fa@,%a.(%a)@]" p_term x p_fol f
  | Bex (x, f) -> fprintf ppf "@[\\ex@,%a.(%a)@]" p_term x p_fol f

and p_term  ppf = function
  | Var s -> fprintf ppf "%s" s
  | Function (s, ts) -> fprintf ppf "%s%a" s p_terms ts
  | Meta s -> fprintf ppf "%s" s
  | Const s -> fprintf ppf "%s" s

and p_terms ppf = function
  | [x] -> fprintf ppf "(%a)" p_term x
  | [] -> ()
  | _ as l -> (
    fprintf ppf "(" ;
    let rec aux ppf = function
      | [x] -> fprintf ppf "%a)" p_term x
      | x :: xs -> fprintf ppf "%a, %a" p_term x aux xs
      | _ -> assert false
    in aux ppf l)
;;

let pout_fol x =
  p_fol std_formatter x;
  fprintf std_formatter "@."
;;

let pout_term x =
  p_term std_formatter x;
  fprintf std_formatter "@."
;;
