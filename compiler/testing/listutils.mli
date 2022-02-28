(** Miscelaneous list functions *)

val map_from_to : ('a -> 'b) -> 'a -> 'a -> ('a -> 'a) -> 'b list ;;
(** [map_from_to f b e succ] Maps a function [f] to a list of elements
    starting from element [b] until element [e] is reached, applying
    function [succ] in turn to get the next element of the list.
*)

val from_to : 'a -> 'a -> ('a -> 'a) -> 'a list ;;
(** [from_to b e succ] Creates a list starting from element [b] until
    element [e] is reached, applying function [succ] in turn to get the next
    element of the list.
*)

val take : int -> 'a list -> 'a list ;;
(** [take n l] Takes the first [n] elements of list [l],
    or the whole list [l], if [l] has less than [n]n elements.
*)

val transpose : 'a list list -> 'a list list ;;
(** [transpose ll] Transposes a list of lists.
    For example, transpose [[1,2],[3,4,5],[]] = [[1,3],[2,4],[5]].
*)

val combine_all : 'a list list -> 'a list list ;;
(** [combine ll] Returns the list of all the lists that can be built by
    taking one element in each list of the list of lists [ll].
    For example,
    combine_all [[1,2],[3,4,5],[6]] =
      [[1,3,6],[1,4,6],[1,5,6],[2,3,6],[2,4,6],[2,5,6]]
*)

val repeat : int -> 'a -> 'a list ;;
(** [repeat n x] Creates a list with [n] element [x].
*)

val flat_map : ('a -> 'b list) -> 'a list -> 'b list ;;
(** [flat_map f l] maps the function [f] on the list [l], and flatten the
    list of results. [flat_map f l] is thus equivalent to
    [List.flatten (List.map f l)], although this implementation of
    [flat_map] is tail recursive.
*)

val unique : ('a -> 'a -> bool) -> 'a list -> 'a list ;;
(** [unique p l] Removes duplicates from a list, provided a binary predicate
    [p].
    Preserves the order of the first appearence.
    For example,
    unique (fun x y -> x = y) [1; 2; 4; 1; 3; 2] =
     [1; 2; 4; 3]
*)

val ith_group: int -> int -> 'a list -> 'a list ;;
(** Separates the list in consecutive groups of the specified size,
    then returns the i_th of such groups starting from 0.
    If there are not enough elements, cuts the group accordingly.
*)
