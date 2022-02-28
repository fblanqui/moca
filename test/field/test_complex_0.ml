let pi = 4.0 *. atan 1.0;;
let thetaj = 2.0 *. (pi /. 3.0);;

let un = one;;

let j =
  add
    (mulext (cos thetaj, one),
     mulext (sin thetaj, i))
;;

let j2 = mul (j, j)
;;

add (un, (add (j, j2))) = zero;;

