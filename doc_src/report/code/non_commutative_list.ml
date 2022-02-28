type t =
  | Elem of string
  | Neutral
  | Nil
  | Inv of t
  | Constr1 of t list
  (* begin
       associative
       neutral left (Neutral)
       inverse left (Inv)
     end *)
  | Constr2 of t list
  (* begin
       idempotent right
     end *)
  | Constr3 of t list
  (* begin
       associative
       idempotent right
     end *)
  | Constr4 of t list
  (* begin
       nilpotent ( left, Nil)
     end *)
  | Constr5 of t list
  (* begin
       associative
       nilpotent ( left, Nil)
     end *)
  | Constr6 of t list
  (* begin
       neutral (Neutral)
       inverse right (Inv)
     end *)
;;

let rec neutral = Neutral

and constr6 l = normalize_constr6 l

and flat_constr6 = function
                     | Constr6 moca_x -> moca_x
                     | moca_x -> [moca_x]

and distrib_constr6 moca_accu l moca_x =
  constr6 (List.rev moca_accu @ (moca_x :: l))

and return_constr6 =
  function
    | [] -> neutral
    | [moca_x] -> moca_x
    | l -> Constr6 l

and normalize_constr6 l =
  match l with
  | [] -> return_constr6 l
  | l -> cons_constr6 [] l

and cons_constr6 moca_accu l =
  match l with
  | (moca_x :: (Neutral :: moca_xs)) -> cons_constr6 moca_accu
      (moca_x :: moca_xs)
  | (Neutral :: (moca_x :: moca_xs)) -> cons_constr6 moca_accu
      (moca_x :: moca_xs)
  | ((Inv moca_x as moca_z) :: moca_xs) ->
      (let moca_ys =
         try delete_constr6 moca_x moca_accu with
         | Not_found -> insert_constr6 moca_z moca_accu in
       cons_constr6 moca_ys moca_xs)
  | ((Inv moca_x as moca_z) :: moca_xs) ->
      (let moca_ys =
         try delete_constr6 moca_x moca_accu with
         | Not_found -> insert_constr6 moca_z moca_accu in
       cons_constr6 moca_ys moca_xs)
  | [] -> return_constr6 (List.rev moca_accu)
  | (moca_x :: moca_xs) -> cons_constr6 (insert_constr6 moca_x moca_accu)
      moca_xs

and insert_constr6 moca_x l =
  match moca_x with
  | Neutral -> l
  | _ -> (match l with
          | [] -> [moca_x]
          | _ -> moca_x :: l)

and delete_constr6 moca_x l =
  match l with
  | [] -> raise Not_found
  | (moca_y :: moca_ys) when compare moca_x moca_y = 0 -> insert_constr6
      neutral moca_ys
  | _ -> raise Not_found

and constr5 l =
  let rev_flat_map f l moca_accu =
     List.rev_append moca_accu
  (List.fold_left
     (fun ll l -> List.rev_append (f l) ll)
     []
     l) in
  let l = List.rev (rev_flat_map flat_constr5 l []) in
  normalize_constr5 l

and flat_constr5 = function
                     | Constr5 moca_x -> moca_x
                     | moca_x -> [moca_x]

and distrib_constr5 moca_accu l moca_x =
  constr5 (List.rev moca_accu @ (moca_x :: l))

and potent_constr5 l1 l1len l maxlen =
  if l1len > maxlen
  then raise Not_found
  else match l with
       | [] -> raise Not_found
       | (moca_y :: moca_ys) ->
           (match matching_heads_constr5 l1 l with
            | Some moca_xs -> moca_xs
            | None -> potent_constr5 (l1 @ [moca_y]) (l1len + 1) moca_ys
                maxlen)

and matching_heads_constr5 l1 l =
  let rec aux moca_ys moca_xs =
    match (moca_ys, moca_xs) with
    | ((moca_x :: moca_xs), (moca_y :: moca_ys)) ->
        if compare moca_x moca_y = 0 then aux moca_xs moca_ys else None
    | ([], _) -> Some (insert_constr5 nil moca_xs)
    | (_, []) -> assert false in
  aux l1 l

and return_constr5 = function
                       | [moca_x] -> moca_x
                       | l -> Constr5 l

and normalize_constr5 l =
  match l with
  | [] -> return_constr5 l
  | l -> cons_constr5 [] l

and cons_constr5 moca_accu l =
  match l with
  | (Constr5 moca_ys :: moca_xs) -> cons_constr5 moca_accu
      (moca_ys @ moca_xs)
  | [] -> return_constr5 (List.rev moca_accu)
  | (moca_x :: moca_xs) -> cons_constr5 (insert_constr5 moca_x moca_accu)
      moca_xs

and insert_constr5 moca_x l =
  match l with
  | (moca_y :: moca_ys) ->
      (try potent_constr5 [moca_x] 1 l ((List.length l + 1) / 2) with
       | Not_found -> moca_x :: l)
  | [] -> [moca_x]
  | _ -> moca_x :: l

and constr4 l = normalize_constr4 l

and flat_constr4 = function
                     | Constr4 moca_x -> moca_x
                     | moca_x -> [moca_x]

and distrib_constr4 moca_accu l moca_x =
  constr4 (List.rev moca_accu @ (moca_x :: l))

and potent_constr4 l1 l1len l maxlen =
  if l1len > maxlen
  then raise Not_found
  else match l with
       | [] -> raise Not_found
       | (moca_y :: moca_ys) ->
           (match matching_heads_constr4 l1 l with
            | Some moca_xs -> moca_xs
            | None -> potent_constr4 (l1 @ [moca_y]) (l1len + 1) moca_ys
                maxlen)

and matching_heads_constr4 l1 l =
  let rec aux moca_ys moca_xs =
    match (moca_ys, moca_xs) with
    | ((moca_x :: moca_xs), (moca_y :: moca_ys)) ->
        if compare moca_x moca_y = 0 then aux moca_xs moca_ys else None
    | ([], _) -> Some (insert_constr4 nil moca_xs)
    | (_, []) -> assert false in
  aux l1 l

and return_constr4 = function
                       | [moca_x] -> moca_x
                       | l -> Constr4 l

and normalize_constr4 l =
  match l with
  | [] -> return_constr4 l
  | l -> cons_constr4 [] l

and cons_constr4 moca_accu l =
  match l with
  | [] -> return_constr4 (List.rev moca_accu)
  | (moca_x :: moca_xs) -> cons_constr4 (insert_constr4 moca_x moca_accu)
      moca_xs

and insert_constr4 moca_x l =
  match l with
  | (moca_y :: moca_ys) ->
      (try potent_constr4 [moca_x] 1 l ((List.length l + 1) / 2) with
       | Not_found -> moca_x :: l)
  | [] -> [moca_x]
  | _ -> moca_x :: l

and constr3 l =
  let rev_flat_map f l moca_accu =
     List.rev_append moca_accu
  (List.fold_left
     (fun ll l -> List.rev_append (f l) ll)
     []
     l) in
  let l = List.rev (rev_flat_map flat_constr3 l []) in
  normalize_constr3 l

and flat_constr3 = function
                     | Constr3 moca_x -> moca_x
                     | moca_x -> [moca_x]

and distrib_constr3 moca_accu l moca_x =
  constr3 (List.rev moca_accu @ (moca_x :: l))

and potent_constr3 l1 l1len l maxlen =
  if l1len > maxlen
  then raise Not_found
  else match l with
       | [] -> raise Not_found
       | (moca_y :: moca_ys) ->
           (match matching_heads_constr3 l1 l with
            | Some moca_xs -> moca_xs
            | None -> potent_constr3 (l1 @ [moca_y]) (l1len + 1) moca_ys
                maxlen)

and matching_heads_constr3 l1 l =
  let rec aux moca_ys moca_xs =
    match (moca_ys, moca_xs) with
    | ((moca_x :: moca_xs), (moca_y :: moca_ys)) ->
        if compare moca_x moca_y = 0 then aux moca_xs moca_ys else None
    | ([], _) -> Some l
    | (_, []) -> assert false in
  aux l1 l

and return_constr3 = function
                       | [moca_x] -> moca_x
                       | l -> Constr3 l

and normalize_constr3 l =
  match l with
  | [] -> return_constr3 l
  | l -> cons_constr3 [] l

and cons_constr3 moca_accu l =
  match l with
  | (Constr3 moca_ys :: moca_xs) -> cons_constr3 moca_accu
      (moca_ys @ moca_xs)
  | [] -> return_constr3 (List.rev moca_accu)
  | (moca_x :: moca_xs) -> cons_constr3 (insert_constr3 moca_x moca_accu)
      moca_xs

and insert_constr3 moca_x l =
  match l with
  | (moca_y :: moca_ys) ->
      (try potent_constr3 [moca_x] 1 l ((List.length l + 1) / 2) with
       | Not_found -> moca_x :: l)
  | [] -> [moca_x]
  | _ -> moca_x :: l

and elem moca_x = Elem moca_x

and constr2 l = normalize_constr2 l

and flat_constr2 = function
                     | Constr2 moca_x -> moca_x
                     | moca_x -> [moca_x]

and distrib_constr2 moca_accu l moca_x =
  constr2 (List.rev moca_accu @ (moca_x :: l))

and potent_constr2 l1 l1len l maxlen =
  if l1len > maxlen
  then raise Not_found
  else match l with
       | [] -> raise Not_found
       | (moca_y :: moca_ys) ->
           (match matching_heads_constr2 l1 l with
            | Some moca_xs -> moca_xs
            | None -> potent_constr2 (l1 @ [moca_y]) (l1len + 1) moca_ys
                maxlen)

and matching_heads_constr2 l1 l =
  let rec aux moca_ys moca_xs =
    match (moca_ys, moca_xs) with
    | ((moca_x :: moca_xs), (moca_y :: moca_ys)) ->
        if compare moca_x moca_y = 0 then aux moca_xs moca_ys else None
    | ([], _) -> Some l
    | (_, []) -> assert false in
  aux l1 l

and return_constr2 = function
                       | [moca_x] -> moca_x
                       | l -> Constr2 l

and normalize_constr2 l =
  match l with
  | [] -> return_constr2 l
  | l -> cons_constr2 [] l

and cons_constr2 moca_accu l =
  match l with
  | [] -> return_constr2 (List.rev moca_accu)
  | (moca_x :: moca_xs) -> cons_constr2 (insert_constr2 moca_x moca_accu)
      moca_xs

and insert_constr2 moca_x l =
  match l with
  | (moca_y :: moca_ys) ->
      (try potent_constr2 [moca_x] 1 l ((List.length l + 1) / 2) with
       | Not_found -> moca_x :: l)
  | [] -> [moca_x]
  | _ -> moca_x :: l

and constr1 l =
  let rev_flat_map f l moca_accu =
     List.rev_append moca_accu
  (List.fold_left
     (fun ll l -> List.rev_append (f l) ll)
     []
     l) in
  let l = List.rev (rev_flat_map flat_constr1 l []) in
  normalize_constr1 l

and flat_constr1 = function
                     | Constr1 moca_x -> moca_x
                     | moca_x -> [moca_x]

and distrib_constr1 moca_accu l moca_x =
  constr1 (List.rev moca_accu @ (moca_x :: l))

and return_constr1 =
  function
    | [] -> neutral
    | [moca_x] -> moca_x
    | l -> Constr1 l

and normalize_constr1 l =
  match l with
  | [] -> return_constr1 l
  | l -> cons_constr1 [] l

and cons_constr1 moca_accu l =
  match l with
  | (Constr1 moca_ys :: moca_xs) -> cons_constr1 moca_accu
      (moca_ys @ moca_xs)
  | (moca_x :: (Neutral :: moca_xs)) -> cons_constr1 moca_accu
      (moca_x :: moca_xs)
  | (Neutral :: (moca_x :: moca_xs)) -> cons_constr1 moca_accu
      (moca_x :: moca_xs)
  | ((Inv moca_x as moca_z) :: moca_xs) ->
      (let moca_ys =
         try delete_constr1 moca_x moca_accu with
         | Not_found -> insert_constr1 moca_z moca_accu in
       cons_constr1 moca_ys moca_xs)
  | ((Inv moca_x as moca_z) :: moca_xs) ->
      (let moca_ys =
         try delete_constr1 moca_x moca_accu with
         | Not_found -> insert_constr1 moca_z moca_accu in
       cons_constr1 moca_ys moca_xs)
  | [] -> return_constr1 (List.rev moca_accu)
  | (moca_x :: moca_xs) -> cons_constr1 (insert_constr1 moca_x moca_accu)
      moca_xs

and insert_constr1 moca_x l =
  match moca_x with
  | Neutral -> l
  | Neutral -> l
  | _ -> (match l with
          | [] -> [moca_x]
          | _ -> moca_x :: l)

and delete_constr1 moca_x l =
  match l with
  | [] -> raise Not_found
  | (moca_y :: moca_ys) when compare moca_x moca_y = 0 -> insert_constr1
      neutral moca_ys
  | _ -> raise Not_found

and nil = Nil

and inv moca_x =
  match moca_x with
  | Inv moca_x -> moca_x
  | Constr1 moca_x ->
      (let dist moca_y = inv moca_y in
       constr1 (List.rev_map dist moca_x))
  | Neutral -> neutral
  | _ -> Inv moca_x
;;

external eq_t : t -> t -> bool =  "%equal"
;;

