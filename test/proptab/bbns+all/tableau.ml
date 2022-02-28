(*
   Simple naive implementation of propositional tableau method
   See Smullyan's 1968 First-Order Logic for good introduction
*)

open Boolean;;

let branch = ref 0;;
let verbose = ref false;;

 
let rec print_formula = function
  | Bor (x, y) -> 
      print_formula x;
      print_string " \\/ ";
      print_formula y;
 
  | Band (x, y) ->
      print_formula x;
      print_string " /\\ ";
      print_formula y;
 
  | Bnot (Batom x)  ->
      print_string ("~" ^ x ^ " ");
  
  | Batom x ->
      print_string (x ^ " ");
       
  | Bfalse  ->
      print_string "false";
        
  | Btrue  ->
      print_string "true";
  
  | Bimplies (x, y) ->
            print_formula x;
      print_string " =>  ";
      print_formula y;

  | _  -> ()
;;

let close_branch b =
  match b with
    | Bfalse ->  true
    | _ ->  print_formula b; flush_all; false
;;


let rec refute f =
  if !verbose then (
  incr branch;
  Printf.printf "Branch %n\n" !branch;);
  match f with
  | Bor (b1, b2) ->
      close_branch b1 && refute b2 
  | _ as b -> close_branch b
;;   

let prove f =
  branch := 0;
  refute (bnot f)
;;

