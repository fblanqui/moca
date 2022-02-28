open Boolean_list;;
open Tableau_list;;
open Unix;;

let v = batom "v"
and w = batom "w"
and x = batom "x"
and y = batom "y"
and z = batom "z"
;;

let noty = bnot y
and notx = bnot x
;;

let bimplies p q = bor [bnot p; q];;

let equiv p q = band [bimplies p q; bimplies q p]
;;

(* let equiv p q = bor [band [p; q), band (bnot p, bnot q));; *)
(*let p = bnot (band (bor (noty, x), bor (notx, y)));;*)

let h = bimplies (bimplies x y) x;;
let peirce = bimplies h  x;;

(* let p2 =  bor (band (x,y), y);; *)
(* let p1 = bnot ( bor (band (x,y), y));; *)

(* let q = band (bor (x, band (w,v)), z);; *)
(* let q2 = bnot q;; *)

(* let s = band (bor (x, z), bor (y,z));; *)
(* let s1 = bor (q, s);; *)

let ex_mid = bor [x; notx];;

let a = batom "a"
and b = batom "b"
and c = batom "c"
and d = batom "d"
and e = batom "e"
;;

Tableau_list.verbose := true;;


let print_time ptimes =
  Printf.printf "%f\n" (ptimes.tms_utime +. ptimes.tms_stime);
  flush_all ()
;;


let test f s =
let aux f s = (
  print_string ("\n**** " ^ s ^ " ****\n");
  flush_all ();

  match (prove f) with
  | true -> print_string "\n Quine \n"
  | false -> print_string "\n Encore perdu! \n";
  flush_all ();
 )

in print_time (Unix.times (aux f s))
;;

let main () = 
  
  test peirce "Peirce's Law";
  
  test ex_mid "Excluded Middle";
  
  let f = bor [x; y] in test f "Falsifiable formula";
  
  let f = equiv b b in test f "One equiv";

  print_time (Unix.times (
  let f = equiv b (equiv c (equiv b c)) in
  test f "Two equivs" ));

  print_time (Unix.times (
 let equiv_suite =
  equiv a (equiv b (equiv c (equiv d (equiv e
 ( equiv a (equiv b (equiv c (equiv d e))))))))
 in
 test equiv_suite "Equivalences" ));
;;
 
main ();;
