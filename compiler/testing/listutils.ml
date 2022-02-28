(* $Id: listutils.ml,v 1.14 2012-04-02 09:27:26 weis Exp $ *)

(** {Miscelaneous list functions} *)

open List
;;


(** Takes first n elements of a list, or the whole list if it has less than n
    elements.
*)
let rec take n l =
   match n, l with
   | 0, _  -> []
   | _, [] -> []
   | n, x :: xs -> x :: take (n - 1) xs
;;

(** Drops first n elements of a list, or the whole list if it has less than n
    elements.
*)
let rec drop n l =
  match n, l with
  | 0, l  -> l
  | _, [] -> []
  | n, _ :: xs -> drop (n - 1) xs
;;

(** Takes the sublist starting from position n and ending in position m.
    (numbering from 0).
    Cuts the sublist accordingly if there are not enough elements.
*)
let sublist n m = fun l -> take (m - n + 1) (drop n l)
;;

(** Separates the list in consecutive groups of the specified size,
    then returns the i_th of such groups starting from 0.
    If there are not enough elements, cuts the group accordingly.
*)
let ith_group i group_size =
  let init, last = group_size * i, group_size * (i + 1) - 1 in
  sublist init last
;;

(** map_filter f p [a1; ...; an] applies function f only to those a1, ..., an
    for which the predicate p holds, and builds the list of results
    [f ai1; ...; f aik].
*)
let rec map_filter f p = function
  | [] -> []
  | x :: xs -> if p x then f x :: map_filter f p xs else map_filter f p xs
;;

let rev_flat_map_end f l accu =
  List.rev_append accu
   (List.fold_left
    (fun ll l -> List.rev_append (f l) ll)
    []
    l)
;;

(** rev_flat_map f [a1 ... an] applies function f, which returns a list, to
    a1,...,an. Then flattens all the lists obtained this way and returns
    the resulting list in reverse order.
    rev_flat_map is tail recursive.
*)
(*let rev_flat_map f l = rev_flat_map_end f l [];;*)

let flat_map_end f l accu =
  List.rev (rev_flat_map_end f l accu)
;;

(** flat_map f [a1 ... an] applies function f, which returns a list, to
    a1,...,an. Then flattens all the lists obtained this way and returns
    the resulting list.
    flat_map is tail recursive.
*)
let flat_map f l = flat_map_end f l [];;

(** Maps a function to a list of elements, provided the start, the end
    and an incrementing function. *)
let rec map_from_to f init last next =
  if init > last then [] else
  f init :: map_from_to f (next init) last next
;;

(** Creates a list from a starting point to and end, given an incrementing
    function. *)
let from_to init last next = map_from_to (fun x -> x) init last next
;;

(** Creates a list containing n times the given element. *)
let rec repeat n elem = if n = 0 then [] else elem :: repeat (pred n) elem
;;

(** Transposes a list of lists.
    For example, transpose [[1,2],[3,4,5],[]] = [[1,3],[2,4],[5]]. *)
let rec transpose = function
  | [] ->  []
  | [] :: xss -> transpose xss
  | (x::xs) :: xss ->
    (x :: map_filter hd (fun x -> x <> []) xss) ::
      transpose (xs :: map_filter tl (fun x -> x <> []) xss)
;;

(** Takes and element of each list to build a new one. Returns all the lists
    built this way.
    For example,
    combine_all [[1,2],[3,4,5],[6]] =
      [[1,3,6],[1,4,6],[1,5,6],[2,3,6],[2,4,6],[2,5,6]]. *)
let rec combine_all lists =
  match lists with
  | [] -> []
  | [xs] -> map (fun x -> [x]) xs
  | xs :: xss ->
    let xss_combination = combine_all xss in
    flat_map (fun x -> map (fun xss -> x :: xss) xss_combination) xs
;;

(* This version is efficient if using lazy evaluation! Otherwise, it has more or
   less a n^2 complexity!

(** Removes from a list, all the elements that satisfy a given predicate *)
let remove_p p x = List.filter (fun y -> not (p x y));;

(** Removes duplicates from a list, provided a predicate.
    Preserves the order of the first appearence. *)
let rec unique p = function
  | [] -> []
  | x :: xs -> x :: unique p (remove_p p x xs)
;;
*)

(* A simpler (and faster?) version for eager languages such as Caml. *)
let unique p l =
  let rec unique preds accu = function
    | [] -> List.rev accu
    | x :: xs ->
      if List.exists (fun p -> p x) preds
      then unique preds accu xs
      else unique (p x :: preds) (x :: accu) xs in
  unique [] [] l
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
