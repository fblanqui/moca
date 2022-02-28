open Lambda_sigma;;
open Lambda_sigma_beta;;
open Lambda_sigma_utils;;

let poly_print oc x = ();;

let print_lambda l = lamsig poly_print poly_print stdout l;;
let print_subst s = psubst poly_print poly_print stdout s;;

let t = beta(lam_to_db(Appl(Lam('x',Var('x')),Lam('x',Var('x'))))) in

let t1 = lambda(bvar Zero) in

print_lambda t; print_newline ();
print_lambda t1; print_newline ();

if t=t1 then print_string "ca marche\n" else print_string "ca marche pas\n";;
