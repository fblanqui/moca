(*
   Simple naive implementation of propositional tableau method
   See Smullyan's 1968 First-Order Logic for good introduction
*)

open Boolean_sharing;;

let branch = ref 0;;
let verbose = ref false;;


let rec print_branch = function
  | Bnot (_, Batom (_, x)) :: xs ->(
      print_string ("~" ^ x ^ " ");
      print_branch xs;
      )
  | Batom (_, x) :: xs ->
      (print_string (x ^ " ");
       print_branch xs;
       )
  | [] -> print_newline (); flush_all ();
  | _ :: xs -> assert false
;;

let close_branch b =
  let rec aux l b = (
    match b with
    | Band (_, Bnot (_, x), y) ->
       if (List.mem x l) then (
         print_branch ((bnot x) :: l); true)
       else aux ((bnot x)::l) y
    | Band (_, x, y) ->
       if 
       List.mem (bnot x) l then (
         print_branch (x :: l); true)
       else aux (x::l) y
    | Bnot (_, x) -> (print_branch ((bnot x) :: l); List.mem x l)
    | x -> (print_branch (x :: l); List.mem (bnot x) l))
  in
  aux [] b
;;


let rec refute f =
  if !verbose then (
  incr branch;
  Printf.printf "Branch %n\n" !branch;);
  match f with
  | Bor (_, b1, b2) ->
      close_branch b1 && refute b2 
  | _ as b -> close_branch b
;;   

let prove f =
  branch := 0;
  refute (bnot f)
;;
