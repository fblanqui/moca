(*
   Simple naive implementation of propositional tableau method
   See Smullyan's 1968 First-Order Logic for good introduction
*)

open Boolean;;
open Format;;

let branch = ref 0;;
let verbose = ref false;;
 
let rec pr_form ppf = function
  | Bor (x, y) ->
      fprintf ppf "@[%a@ \\/@ %a @]@." pr_form x pr_form y
  | Band (x, y) ->
      fprintf ppf "@[%a@ /\\@ %a @]@?" pr_form x pr_form y
  | Bimplies (x, y) ->
      fprintf ppf "@[%a@ =>@ %a @]@?" pr_form x pr_form y
  | Bnot x ->
      fprintf ppf "@[~%a@]@?" pr_form x 
  | Batom x -> 
      fprintf ppf "@[%s@]@?" x
  | Bfalse ->
      fprintf ppf "@[false@]@?"
  | Btrue ->
      fprintf ppf "@[true @]@?"
;;

let print_form f =
  let ppf = std_formatter in
  fprintf ppf "@[%a@]@." pr_form f
;;


let extract_model form_list =
  let ppf = std_formatter in
  let print_interpretation f =
    match f with
    | Bnot x ->  fprintf ppf "@[%a@ = 0@]@." pr_form x
    | _ as x ->  fprintf ppf "@[%a@ = 1@]@." pr_form x
  in
  List.iter print_interpretation form_list
;;

let close_branch b =
  let rec aux l b = 
    match b with
    | Band (Bfalse, y) | Band (y, Bfalse) -> true       
    | Band (x, y) ->
        List.mem (bnot x) l ||  aux (x :: l) y
    | Bfalse ->  true
    | _ as x -> List.mem (bnot x) l || 
      (extract_model l ; false)
  in
  aux [] b
;;


let rec refute f =
  if !verbose then (
  incr branch;
  Printf.printf "\n Branch %n\n" !branch;);
  match f with
  | Bor (b1, b2) ->
      close_branch b1 && refute b2 
  | _ as b -> close_branch b
;;   

let prove f =
  branch := 0;
  let notf = bnot f in
  ( pr_form std_formatter notf;
  refute (bnot f))
;;

