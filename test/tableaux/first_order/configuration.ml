let get_maxdepth, set_maxdepth =
  let maxdepth = ref 5 in
  (fun () -> !maxdepth),
  (fun i -> maxdepth := i)

let verbose = ref false;;
