
let newsymb , reset_newsymb =
  let i = ref 0 and tvar = "x" in
  (fun () -> (
           incr i;
           tvar ^ string_of_int !i)),
  (fun () -> i := 0)

;;

let new_tsymb, reset_new_symb =
  let ti = ref 0 and tvar = "t" in
  (fun () -> (
           incr ti;
           tvar ^ string_of_int !ti)),
  (fun () -> ti := 0)
;;
