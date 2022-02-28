open Complex_1;;

open Gentest;;

testing "Complex 1"
;;

testl 1
  (let o = rOne in
   let j = iOne in
   let j2 = cMul (cElt(rZero, j), cElt(rZero, j)) in
   cAdd (cElt(o, iZero),
         cAdd( cElt(rZero, j),
                j2)) = (cElt(rZero, j)))
;;


testl 2
  (let opp = cOpp cROne in
   let j = iOne in
   let j2 = cMul (cElt(rZero, j), cElt(rZero, j)) in
   j2 = opp)
;;
