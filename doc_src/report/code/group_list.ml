type 'a t =
  | Unit
  | Gen of 'a
  | Opp of 'a t
  | Mult of 'a t list
  (* begin
       associative
       neutral (Unit)
       inverse (Opp)
     end *)
;;

let rec opp moca_x =
  match moca_x with
  | Opp moca_x -> moca_x
  | Mult moca_x ->
      (let dist moca_y = opp moca_y in
       mult (List.rev_map dist moca_x))
  | Unit -> unit
  | _ -> Opp moca_x

and mult l =
  let rev_flat_map f l moca_accu =
     List.rev_append moca_accu
  (List.fold_left
     (fun ll l -> List.rev_append (f l) ll)
     []
     l) in
  let l = List.rev (rev_flat_map flat_mult l []) in
  normalize_mult l

and flat_mult = function
                  | Mult moca_x -> moca_x
                  | moca_x -> [moca_x]

and distrib_mult moca_accu l moca_x =
  mult (List.rev moca_accu @ (moca_x :: l))

and return_mult = function
                    | [] -> unit
                    | [moca_x] -> moca_x
                    | l -> Mult l

and normalize_mult l =
  match l with
  | [] -> return_mult l
  | l -> cons_mult [] l

and cons_mult moca_accu l =
  match l with
  | (Mult moca_ys :: moca_xs) -> cons_mult moca_accu (moca_ys @ moca_xs)
  | (moca_x :: (Unit :: moca_xs)) -> cons_mult moca_accu (moca_x :: moca_xs)
  | (Unit :: (moca_x :: moca_xs)) -> cons_mult moca_accu (moca_x :: moca_xs)
  | ((Opp moca_x as moca_z) :: moca_xs) ->
      (let moca_ys =
         try delete_mult moca_x moca_accu with
         | Not_found -> insert_mult moca_z moca_accu in
       cons_mult moca_ys moca_xs)
  | [] -> return_mult (List.rev moca_accu)
  | (moca_x :: moca_xs) -> cons_mult (insert_mult moca_x moca_accu) moca_xs

and insert_mult moca_x l =
  match moca_x with
  | Unit -> l
  | _ -> (match l with
          | [] -> [moca_x]
          | _ -> moca_x :: l)

and delete_mult moca_x l =
  match l with
  | [] -> raise Not_found
  | (moca_y :: moca_ys) when compare moca_x moca_y = 0 -> insert_mult unit
      moca_ys
  | _ -> raise Not_found

and unit = Unit

and gen moca_x = Gen moca_x
;;

external eq_t : 'a t -> 'a t -> bool =  "%equal"
;;

