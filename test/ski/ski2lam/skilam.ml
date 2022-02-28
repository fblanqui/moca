open Utils;;

type tlam =
  | Var of string
  | Abst of string * tlam
  | App of tlam * tlam
;;   
 
let rec lam_is_free x t =
  match t with
  |  Var y when x = y -> true
  | Var y -> false
  | Abst (s, t) when x = s ->  false
  | Abst (s, t) -> lam_is_free x t
  | App (t, u) -> lam_is_free x t || lam_is_free x u
;;

let rec lam_fv  = function
  | Var x -> [x]
  | App (x, y) -> union (lam_fv x) (lam_fv y)
  | Abst (x, y) -> remove_all x (lam_fv y)
;;

type tski =
  | Svar of string
  | S
  | K
  | I
  | Sapp of tski * tski
;;
