open Nilpotence_list
;;

open Gentest
;;

testing "Nilpotence_list"
;;

(* Tests generated automatically that once failed *)

testi 0
(let x = proj (proj one) in
 let y = one in
 let z = proj (nprod [one; zero; one]) in
 nprod [x; y; y; z] = nprod [x; zero; z])
;;
