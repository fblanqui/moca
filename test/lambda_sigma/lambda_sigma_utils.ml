open Printf;;

open Lambda_sigma;;

let rec int_of_nat =function
  | Zero -> 0
  | Suc n -> succ (int_of_nat n)
;;

let outnat oc n = fprintf oc "%i" (int_of_nat n);;


let rec lamsig pvar psymb oc =

  let rec_lamsig oc l = lamsig pvar psymb oc l
  and rec_subst oc s = psubst pvar psymb oc s in
  
  function
    | Fvar var -> fprintf oc "Fvar %a" pvar var
    | Symb symb -> fprintf oc "Symb %a" psymb symb
    | Bvar n -> fprintf oc "Bvar %a" outnat n
    | Lambda lam -> fprintf oc "lam %a" rec_lamsig lam
    | App (lam1,lam2) -> fprintf oc "(%a %a)" rec_lamsig lam1 rec_lamsig lam2
    | Subst (lam,subs) -> fprintf oc "(%a[%a])" rec_lamsig lam rec_subst subs

and psubst pvar psymb oc = 
  
  let rec_lamsig oc l = lamsig pvar psymb oc l
  and rec_subst oc s = psubst pvar psymb oc s in
  
  function
    | Id -> fprintf oc "Id"
    | Inst (lam,subs)-> fprintf oc "%a.%a" rec_lamsig lam rec_subst subs
    | Comp (subs1,subs2) -> fprintf oc "%ao%a" rec_subst subs1 rec_subst subs2
;;
