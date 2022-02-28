
type 'a sequent_set = private
   | Trivial
   | Closed
   | Positive of 'a
   | Negative of 'a
   | Graft of 'a sequent_set * 'a sequent_set
     (*
       associative commutative neutral (Trivial) absorbent (Closed)
       distributive (Union)
       rule Graft (Positive p, Negative q) (* when p = q *) -> Closed
       rule Graft (Negative p, Positive q) (* when p = q *) -> Closed
       rule Graft (Positive p, Graft (Negative q, r)) (* when p = q *) -> Closed
       rule Graft (Negative p, Graft (Positive q, r)) (* when p = q *) -> Closed
     *)
   | Union of 'a sequent_set * 'a sequent_set
     (*
       associative commutative neutral (Closed) absorbent (Trivial)
       distributive (Graft)
    *)
;;
val closed : 'a sequent_set
val trivial : 'a sequent_set
val positive : 'a -> 'a sequent_set
val graft : 'a sequent_set * 'a sequent_set -> 'a sequent_set
val negative : 'a -> 'a sequent_set
val union : 'a sequent_set * 'a sequent_set -> 'a sequent_set
