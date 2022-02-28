(*
   Simple naive implementation of propositional tableau method
   See Smullyan's 1968 First-Order Logic for good introduction
*)

open Boolean_list;;
open Format;;

let branch = ref 0;;
let verbose = ref false;;


let rec pr_bool ppf = function
  | Bor l ->
      fprintf ppf "@[ %a @]@." pr_bin (`Or,l)
  | Band l ->
      fprintf ppf "@[ %a @]@." pr_bin (`And,l)
  | Bimplies (x, y) ->
      fprintf ppf "@[(@ %a@ )@]@?" pr_bin (`Implies, x :: [y])
  | Bequiv l ->
      fprintf ppf "@[(@ %a@ )@]@?" pr_bin (`Equiv, l)
  | Bnot x -> fprintf ppf "~(%a)@?"  pr_bool x
  | Batom x -> fprintf ppf "%s@?" x
  | Btrue -> fprintf ppf "true@?"
  | Bfalse -> fprintf ppf "false@?"

and pr_bin ppf (s,l) =
  match l with
  | [x] -> fprintf ppf "%a" pr_bool x
  | x :: xs -> fprintf ppf "@[ (%a)@ %a@ (%a)@]"
        pr_bool x pr_con s pr_bin (s,xs)
  | _ -> assert false

and pr_con ppf = function
  | `Or -> fprintf ppf "\\/"
  | `And -> fprintf ppf "/\\"
  | `Implies -> fprintf ppf "=>"
  | `Equiv -> fprintf ppf "<=>"
;;

let close_branch b =
  (*if !verbose then pr_bool std_formatter b;*)
    match b with
    | Band l -> (       
        let rec aux l =
          match l with
          | (Bnot e) :: es ->
              List.mem e es || aux es
          | e :: es ->
              List.mem (bnot e) es || aux es
          | [] -> false
        in aux l
          )
    | _ -> assert false
;;


let rec refute f =
  if !verbose then pr_bool std_formatter f;
  match f with
  | Bfalse -> true
  | Bor l ->
      let rec close_branches l =
        match l with
        | b :: bs ->
            close_branch b && close_branches bs
        | [] -> true
      in close_branches l
  | Band l -> close_branch f
  | _  -> false
;;   

let prove f =
  branch := 0;
  refute (bnot f)
;;

