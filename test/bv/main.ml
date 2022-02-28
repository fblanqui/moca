open Bv;;
open Format;;



let rec pr_struct ppf = function
  | Sunit ->  fprintf ppf "o"
  | Satom x ->  fprintf ppf "%s" x
  | Sseq (x, y) ->
      fprintf ppf "{%a;@ %a}" pr_struct x  pr_struct y 
  | Spar (x, y) ->
      fprintf ppf "[%a,@ %a]" pr_struct x  pr_struct y 
  | Scopar (x ,y) ->
    fprintf ppf "(%a,@ %a)" pr_struct x  pr_struct y
  | Sneg x ->
      fprintf ppf "~@ %a" pr_struct x
and pr_structure ppf f =
    fprintf ppf "@[ %a @]@." pr_struct f
     
let print f =
  pr_structure std_formatter f
;;


let a = satom "a"
and b = satom "b"
and c = satom "c"
and d = satom "d"
and e = satom "e"
and f = satom "f"
;;


let prove = print;;

let s = scopar (sneg b, c)
and t = scopar (sneg a, sneg c);;
let u = spar (t, s) ;;
let f1  = spar (a, spar (b, u))
;; 

let main () =
  print s; print t; print u;
  print f1
;;
  

main ();;
  
