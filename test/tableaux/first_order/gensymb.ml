
let get_new_meta  =
  let basename = "meta" 
  and i = ref 0 in
  fun () -> (incr i; basename ^ string_of_int !i)

let get_new_const  =
  let basename = "const" 
  and i = ref 0 in
  fun () -> (incr i; basename ^ string_of_int !i)
