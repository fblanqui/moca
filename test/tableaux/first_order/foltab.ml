open Pprint
open Fol
open Format
;;

exception Predicate_arity
exception Unification_impossible
exception Occurs_check
exception Not_closeable
;;

let rec occurs x t =
  if x = t then true
  else match t with
    | Function (_, ts) -> List.fold_left (fun y z -> y or (occurs x z)) false ts
    | _ -> false
;;

let rec get_inst t u =
  match t, u with
    | Meta _, Meta _ -> [t, u]
    | Meta _, _ ->
      if not (occurs t u) then [t, u]
      else raise Occurs_check
    | _, Meta _ ->
      if not (occurs u t) then [u, t]
      else raise Occurs_check
    | Function (f, fargs), Function (g, gargs) when f = g ->
      List.fold_left2 (fun l x y -> (get_inst x y) @ l) [] fargs gargs
    | _ -> raise Unification_impossible
;;

let get_possible_inst ts (p, us) =
  let rec aux ts us =
    match ts, us with
      | [] , [] -> []
      | x :: xs, y :: ys -> (get_inst x y) @ (aux xs ys)
      | [], _ | _, [] -> raise Predicate_arity
  in aux ts us
;;

let unify_branch (pos, neg) =
  let first_equal (x1, y1) (x2, y2) = x1 = x2 in
  let unifl = List.fold_left
    (fun x (y, z) ->
      let inst = match List.filter (first_equal (y, z)) neg with
        | [] -> []
        | _ as l -> List.fold_left
          (fun l1 e -> try (get_possible_inst z e) :: l1 with _ -> l1) [] l
      in inst @ x ) [] pos
  in match unifl with
    | [] -> raise Not_closeable
    | _ -> unifl
;;

let pre_unify b =
  (* b is a and comb of atoms *)
  (* produces two lists one binding positive occurrences of p to*)
  (* its subterms, the other one doing the same for negative occurrences
  *)
  let rec potentially_closeable_pairs b =
    match b with
      | Band (Bnot (Prop(_, p, (_t :: _ts as l))), f) ->
        let pos, neg = potentially_closeable_pairs f in
        pos, ((p, l) :: neg)

      | Band (Prop(_, p, (_t :: _ts as l)), f) ->
        let pos, neg = potentially_closeable_pairs f in
        ((p, l) :: pos), neg

      | Bnot (Prop(_, p, (t :: ts as l))) -> ([], [(p,l)])
      | Prop(_, p, (t :: ts as l)) -> ([p,l], [])
      | _ -> assert false
  in potentially_closeable_pairs b
;;

let print_unif ppf l =
  Format.fprintf ppf "@[<v 0>Unifier:@ \
                             --------@ \
                      %a@]"
    (fun ppf l -> List.iter
      (fun (x, y) -> Format.fprintf ppf "@[%a@ :=@ %a@]@ " p_term x p_term y) l)
    l
;;

let print_branches l =
  if !Configuration.verbose then begin
  let i = ref 0 in
  List.iter (fun z ->
    incr i;
    Format.printf "@[<v 0>Branch %d@ %a@]" !i
      (fun ppf l ->
        let j = ref 0 in
        List.iter (fun y ->
          incr j;
          Format.fprintf ppf "@[%d@ ::@ %a@]@ " !j print_unif y) l) z)
    l
  end
;;

let close_branch b = unify_branch (pre_unify b) ;;

let add e l = if List.mem e l then l else l @ [e];;

let rec union e = function
  | x :: xs -> let e2 = union e xs in if List.mem x e then e2 else x :: e2
  | _ -> e
;;

let are_compatible u1 u2 =
  let rec is_compatible (x, y) = function
    | (t, u) :: ys ->
        if x = t then (y = u) && is_compatible (x, y) ys
        else is_compatible (x, y) ys
    | [] -> true
  in List.fold_left (fun b e -> b && is_compatible e u2) true  u1
;;

let compatible_with_current_subst l s =
  List.fold_left (fun b l1 -> b && are_compatible l l1) true s
;;

(* l is the list of the list of possible closing substitutions per
   branch
*)
let add_compatibles slist substs =
  match substs with
    | [] -> List.map (fun y -> y :: []) slist
    | _ as k ->
      let aux substs s =
        List.fold_left (fun sl su ->
          if are_compatible s su then add (union s su)  sl else sl) [] substs
       in List.map (aux k) slist
;;

let find_global_subst l =
  List.fold_left (fun x y -> List.flatten (add_compatibles y x)) [] l;;

let rec close_tableau t =
  match t with
 (* a single branch *)
  | Band (_, _) ->  [close_branch t]
 (* multiple branches *)
  | Bor (b1, b2) -> (close_branch b1) :: (close_tableau b2)
  | _ -> assert false (* should not happen! *)
;;

let is_unifiable_debug ts =
  let s = find_global_subst ts in
  match s with
    | [] -> false
    | [x] -> printf "%a" print_unif x;  true
    | x :: xs as l ->
      List.iter (fun x -> Format.printf "@[<v 0>%a@ @]" print_unif x) l;
      true
;;

let is_unifiable ts = (find_global_subst ts)  <> []

let refute_debug f =
  let no_proof_found () =
    Format.printf "This formula is not a theorem (under depth %d).@."
      (Configuration.get_maxdepth ())
  in
  let proof_found () =
    Format.printf "This formula a propositional theorem.@."
  in
  Format.printf "@[<v 0>@ ========@ \
                          Refuting@ \
                          ========@ \
                          %a@ @ @]" p_fol f;
  match f with
    | Bfalse -> proof_found (); true;
    | Btrue -> no_proof_found (); false;
    | _ ->
      begin
        try
          let s = close_tableau f in
          print_branches s;
          if not (is_unifiable s) then (no_proof_found (); false)
          else true
         with
           | Not_closeable -> (no_proof_found (); false)

      end
;;

let refute f =
  match f with
    | Bfalse -> true;
    | Btrue -> false;
    | _ ->
      begin
        try
          let s = close_tableau f in is_unifiable s
        with
          | Not_closeable -> false
      end
;;

let prove f = refute (bnot f);;
