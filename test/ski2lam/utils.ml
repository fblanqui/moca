let rec union xs ys =
  match xs, ys with
  | [], _ -> xs
  | _, [] -> ys
  | z :: zs, _ -> if List.mem z ys then union zs ys
               else z :: (union zs ys)
;;

let rec remove_all x = function
  | [] -> []
  | y :: ys when x = y -> ys
  | y :: ys -> y :: (remove_all x ys)
;;


