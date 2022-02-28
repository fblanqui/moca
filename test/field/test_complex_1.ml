open Complex_1;;

let un = one;;

let j =
  add
    (real (-0.5),
     mul (real (sqrt 3.0 /. 2.0), i))
;;

let j2 = mul (j, j)
;;

add (un, (add (j, j2))) = zero;;

