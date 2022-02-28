open Boolean;;
open Tableau;;
open Unix;;


let ltime = ref (Unix.times ());;

let v = batom "v"
and w = batom "w"
and x = batom "x"
and y = batom "y"
and z = batom "z"
;;

let noty = bnot y
and notx = bnot x
;;


let equiv p q = bequiv (p, q)
;;

let p = bnot (band (bor (noty, x), bor (notx, y)));;

let h = bimplies ( bimplies (x, y), x);;
let peirce = bimplies (h  , x);;

(* Let p2 =  bor (band (x,y), y);; *)
(* let p1 = bnot ( bor (band (x,y), y));; *)

(* let q = band (bor (x, band (w,v)), z);; *)
(* let q2 = bnot q;; *)

(* let s = band (bor (x, z), bor (y,z));; *)
(* let s1 = bor (q, s);; *)

let ex_mid = bor (x, notx);;

let a = batom "a"
and b = batom "b"
and c = batom "c"
and d = batom "d"
and e = batom "e"
;;


(* Evaluate time to canonical form *)
let difftime now orig =
  (now.tms_utime +. now.tms_stime) -. (orig.tms_utime +. now.tms_stime)
;;

let moca_time form =
  let last_time = !ltime in
  ( ltime := Unix.times ();
   difftime !ltime last_time, form )
;;

Tableau.verbose := true;;

let print_formula f =
  let rec aux ob =  function
  | Bor (x, y) -> 
    Printf.fprintf ob "( %a \\/ %a )" aux x aux y
  | Band (x, y) ->
    Printf.fprintf ob "( %a /\\ %a )" aux x aux y
  | Bnot (Batom x)  ->
      Printf.fprintf ob "~ %s" x ;
  | Batom x ->
      Printf.fprintf ob "%s" x;
  | Bfalse  ->
      Printf.fprintf ob "false";
  | Btrue  ->
      Printf.fprintf ob "true";
  | Bimplies (x, y) ->
      Printf.fprintf ob "( %a => %a )" aux x aux y
   | Bequiv (x, y) ->
      Printf.fprintf ob "(%a <=> ( %a ))" aux x aux y
  | _  -> assert false
 in 
  aux (out_channel_of_descr Unix.stdout) f;
  flush_all ();
  print_newline ();
  
  
;;

let hilcount = ref 0;;
let haxiom () = 
  incr hilcount; "Hilbert's axiom " ^ (string_of_int !hilcount)
;;

(** Exercices from Basic Proof Theory p.42-34 *)
let mytauto =
  [ (moca_time peirce, "Peirce's Law");
    (moca_time ex_mid, "Excluded Middle");
    (moca_time (bimplies (bimplies (b, c),
                 bimplies ( bimplies (a, b),
                            bimplies (a, c))))
       , " B Axiom" );
    (moca_time (bimplies (bimplies (a, bimplies (b, c)),
                          bimplies (b, bimplies (a, c))))
       , "C Axiom");
    (moca_time (bimplies (bimplies (a, bimplies (a, b)),
                          bimplies (a, b)))
       , "W Axiom");
    (moca_time (bimplies (bimplies (bnot (bnot a), bnot (bnot b)),
                          bnot (bnot (bimplies (a, b)))))
     , "TS 2.1.8D");
  ]
;;

let falseformulas =
  [ (moca_time (bor (a, b)), "Disprove 1");
    (moca_time (band (a ,b)), "Disprove 2")
  ]



let print_time ptimes =
  Printf.printf "%f\n" (ptimes.tms_utime +. ptimes.tms_stime);
  flush_all ()
;;


let print_exec_time t1 t2 =
  Printf.printf "Normalization time: %f s\n" t1;
  Printf.printf "Proof time: %f s\n" t2;
  flush_all();
;;

let test t f s =
ltime := Unix.times ();
let aux f s = (
  print_string ("\n**** " ^ s ^ " ****\n");
  flush_all ();

  match (Tableau.prove f) with
  | true -> print_string "\n \\o/\\o/\\o/ Proof Found \n"
  | false -> print_string "\n :-((((( Proof NOT Found\n";
  flush_all ();
 )
in aux f s;
print_exec_time t (difftime (Unix.times ()) !ltime);
;;

let rec gen_sym n =
  if n = 1
  then [batom ("x"^string_of_int(n))]
  else (batom ("x"^string_of_int(n))) :: (gen_sym (n-1))
;;

let equiv_test n = 
  let symb_list = gen_sym n in
  let d_symb_list = symb_list @ symb_list in
    List.fold_right
       (fun x y -> equiv x y)
       (List.tl d_symb_list)
       (List.hd d_symb_list)
;;
  
let testlist = List.iter (fun ((t,f),y) -> test t f y);;

let main () = 
  
(* Hilbert's axioms, as found in
   Basic Proof Theory by Schwichtenberg and Troelstra, p. 51 *)

let hilberts_axioms = [
  moca_time (bimplies (a, bimplies (b, a)));
  moca_time (bimplies (bimplies (a, bimplies (b, c)),
             (bimplies (bimplies (a, b), bimplies (a, c)))));
  moca_time (bimplies (a, bor (a, b)));
  moca_time (bimplies (b, bor (a,b)));
  moca_time (bimplies (bimplies (a, c),
            bimplies ( bimplies (b, c),
                       bimplies (bor (a, b), c))));
  moca_time (bimplies (band (a,b), a));
  moca_time (bimplies (band (a,b), b));
  moca_time (bimplies (a, bimplies (b, band (a, b))));
]
in

let htautologies =
  List.map (fun x -> (x, haxiom ()))  hilberts_axioms
in

  testlist htautologies;
  
 testlist mytauto;
  testlist falseformulas;

  (* Some equivalence-based tautologies *)
  (* 8 -> 8s *)
  (* 9 -> 86 s *)
  (* 10 -> 883 s *)

    testlist [(moca_time (equiv_test 1), "e1" ); 
    (moca_time (equiv_test 2), "e2" );
    (moca_time (equiv_test 3), "e3")]
;
 testlist [
    (moca_time (equiv_test 5), "e5");
    (moca_time (equiv_test 6), "e6")
];

 testlist [
    (moca_time (equiv_test 7), "e7");];
 testlist [
   (moca_time (equiv_test 8), "e8");
];

(*   testlist [ *)
(* (moca_time (equiv_test 9), "e9")]; *)
(* testlist [ *)
(*     (moca_time (equiv_test 10), "e10");]; *)

;;
 
main ();;
