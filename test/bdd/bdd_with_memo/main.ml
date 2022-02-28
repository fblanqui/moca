open Bdd;;
open Queens;;

let debug = ref false;;

(****************************
let v = batom (Atom.make "v")
and w = batom (Atom.make "w")
and x = batom (Atom.make "x")
and y = batom (Atom.make "y")
and z = batom (Atom.make "z")
and a = batom (Atom.make "a")
and b = batom (Atom.make "b")
and c = batom (Atom.make "c")
;;

let noty = bnot y
and notx = bnot x
;;

let equiv p q = band (bimplies (p, q), bimplies (q, p));;

(* let p = bnot (band (bor (noty, x), bor (notx, y)));; *)
let rec gen_sym n =
  if n = 1 then [batom (Atom.make ("x" ^ string_of_int n))]
  else batom (Atom.make ("x" ^ string_of_int n)) :: gen_sym (n - 1)
;;

let equiv_test n =
  let symb_list = gen_sym n in
  let d_symb_list = symb_list @ symb_list in
  List.fold_right
    (fun x y -> equiv x y)
    (List.tl d_symb_list)
    (List.hd d_symb_list)
;;


let h = bimplies (bimplies (x, y), x);;
let peirce = bimplies (h, x);;

(* Let p2 =  bor (band (x,y), y);; *)
(* let p1 = bnot ( bor (band (x,y), y));; *)

(* let q = band (bor (x, band (w,v)), z);; *)
(* let q2 = bnot q;; *)

(* let s = band (bor (x, z), bor (y,z));; *)
(* let s1 = bor (q, s);; *)

let ex_mid = bor (x, notx);;

(*let x = Dt.batom (Atom.make "x") and y = Dt.batom (Atom.make "y");;
let pdt = Dt.band (x, y);;
*)
let x = Bdd.batom (Atom.make "x") and y = Bdd.batom (Atom.make "y");;
let pbdd = Bdd.band (x, y);;

let mytauto =
  [ (peirce, "Peirce's Law");
    ( ex_mid, "Excluded Middle");
    ( (bimplies
        (bimplies (b, c),
         bimplies
           (bimplies (a, b),
            bimplies (a, c))))
       , "B Axiom" );
    ( (bimplies
        (bimplies (a, bimplies (b, c)),
         bimplies (b, bimplies (a, c))))
       , "C Axiom");
    ( (bimplies
        (bimplies (a, bimplies (a, b)),
         bimplies (a, b)))
       , "W Axiom");
    ( (bimplies
        (bimplies (bnot (bnot a), bnot (bnot b)),
         bnot (bnot (bimplies (a, b)))))
       , "TS 2.1.8D");
  ]
;;

let test n f  =
  print_string ("\n**** " ^ n ^ " ****\n");
  flush_all ();
  if !debug then begin Queens.trace_bdd f; flush_all (); end;

  match f with
  | Btrue -> print_string "\n \\o/\\o/\\o/ Proof Found \n"
  | _ -> print_string "\n :-((((( Proof NOT Found\n";
  flush_all ();
;;

let testlist () = List.iter (fun (f, n) -> test n f) mytauto;;
testlist ();;
**************************)

let umsg = "Usage: nqueens <n>";;
let argspec = [];;

let queen s =
  let ppf = Format.std_formatter in
  Queens.get_all_solutions ppf (int_of_string s)
;;

let main () =
  try
    Arg.parse (Arg.align argspec) queen umsg
  with x -> prerr_endline "Error\n"; raise x
(*   testlist [ (make_queens 1, "1 queen") ];  *)
(*   testlist [ (make_queens 2, "2 queens") ];  *)
(*    testlist [ (make_queens 3, "3 queens") ];   *)
(* (\*     testlist [ (make_queens 4, "4 queens") ];  *\) *)
(* (\*   testlist [((make_queens 5), "5 queens") ];  *\) *)
(*   (\* testlist [(Nqueens.make_queens 4, "4queens") ]; *\) *)
(*   get_all_solutions 1; *)
(*   get_all_solutions 4; *)
(*   get_all_solutions 5; *)
(*   get_all_solutions 6; *)
(*   get_all_solutions 7; *)
;;

main ()
;;
