open Pprint;;
open Fol;;

Configuration.set_maxdepth 1;;

let x = "x"
and y = "y"
and p = "p"
and q = "q"
;;

let vx = Var x
and vy = Var y
and ta = Const "a"
and tb = Const "b"
and tc = Const "c"
;;

let prop (x, y) = Fol.prop ([], x, y) ;;
let p2 = ball (vx, ball (vy, prop (p, [vx; vy])))
and q2 = bex (vx, bex (vy, prop (p, [vx; vy]))) ;;

let r = bimplies (p2, q2) ;;
let notr = bnot r;;
let p1 = prop (p, [vx])
and q1 = prop (q, [vx])
;;

let z = ball (vx, bor (p1, q1));;
let pr1 = bor (prop (p, [ta]), prop (p, [tb]));;
let pr2 = bor (prop (p, [ta]), bnot (prop (p, [tb])));;
let pr3 = bor (ball (vx, prop (p, [vx])), bnot (prop (p, [tb])));;
let pr4 = band (pr3, bor (ball (vx, prop (p, [vx])), bnot (prop (p, [ta]))));;
let pr5 = band (pr3, pr3);;
let pr6 = bimplies (ball (vx, prop (p, [vx])), bex (vx, prop (p, [vx])));;

let main () =
  Gentest.testing "first order tableaux with iterative deepening.";
(*  Format.printf
    "@[<v 0> Moca tableaux for first-order classical logic@ \
             =============================================@]@."; *)
  let i = ref (-1) in
  List.iter
    (fun (formula, truth_value) ->
      incr i;
      Gentest.testi !i (Foltab.prove formula = truth_value);
    )
    [ (pr1, false);
      (pr2, false);
      (pr3, true);
      (pr4, true);
      (pr5, true);
      (pr6, true);
    ];
;;

main ()
;;
