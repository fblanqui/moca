(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2012,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id: relation.ml,v 1.22 2012-06-04 13:01:22 weis Exp $ *)

(** {3 Operations on relations of relational types.} *)

open Parsetree
;;

(** {6 Test functions on the type relation.} *)

let is_rewrite r =
  match r.prel_desc with Rewrite _ -> true | _ -> false
and is_not_rewrite r =
  match r.prel_desc with Rewrite _ -> false | _ -> true
and is_commutative r =
  match r.prel_desc with Commutative _ -> true | _ -> false
and is_associative r =
  match r.prel_desc with Associative _ -> true | _ -> false
and is_absorbent r =
  match r.prel_desc with Absorbent _ -> true | _ -> false
and is_absorbing r =
  match r.prel_desc with Absorbing _ -> true | _ -> false
and is_neutral r =
  match r.prel_desc with Neutral _ -> true | _ -> false
and is_idempotent r =
  match r.prel_desc with Idempotent _sd -> true | _ -> false
and is_nilpotent r =
  match r.prel_desc with Nilpotent _ -> true | _ -> false
and is_involutive r =
  match r.prel_desc with Involutive -> true | _ -> false
and is_inverse r =
  match r.prel_desc with Inverse _ -> true | _ -> false
and is_distributive r =
  match r.prel_desc with Distributive _ -> true | _ -> false
;;

(** {6 Test functions on lists of relations.} *)

let predicate_on_relations p relations =
  match relations.prels_desc with
  | Prels_none | Prels_commented _ -> false
  | Prels_begend rls -> List.exists p rls
;;

let has_commutative = predicate_on_relations is_commutative
and has_associative = predicate_on_relations is_associative
and has_idempotent = predicate_on_relations is_idempotent
and has_absorbing = predicate_on_relations is_absorbing
and has_absorbent = predicate_on_relations is_absorbent
and has_neutral = predicate_on_relations is_neutral
and has_nilpotent = predicate_on_relations is_nilpotent
;;

let has_idempotent_and_nilpotent relations =
  has_nilpotent relations && has_idempotent relations
;;

(** {6 Access function on lists of relations.} *)
let get_relation_list relations =
  match relations.prels_desc with
  | Prels_none | Prels_commented _ -> []
  | Prels_begend rls -> rls
;;

let gather_on_relations gather relations =
  match relations.prels_desc with
  | Prels_none | Prels_commented _ -> []
  | Prels_begend rls -> gather rls
;;

let rec gather_rewrite = function
  | { prel_desc = Rewrite (p, e); prel_loc = _; } :: l ->
    (p, e) :: gather_rewrite l
  | _ :: l -> gather_rewrite l
  | [] -> []
;;

let rec gather_distributive = function
  | { prel_desc = Distributive (sd, g, gopt, dir); prel_loc = _; } :: l ->
    (sd, g, gopt, dir) :: gather_distributive l
  | _ :: l -> gather_distributive l
  | [] -> []
;;

let rewrite = gather_on_relations gather_rewrite
and distributive = gather_on_relations gather_distributive
;;

let rec find_relation p  = function
  | [] -> raise Not_found
  | r :: _ when p r -> r
  | _ :: l -> find_relation p l
;;

let get_opt_of_relations get relations =
  match relations.prels_desc with
  | Prels_none | Prels_commented _ -> None
  | Prels_begend rls -> get rls
;;

let rec get_commutative = function
  | { prel_desc = Commutative comparison_f; prel_loc = _; } :: _ ->
    comparison_f
  | _ :: l -> get_commutative l
  | [] -> None
;;

let rec get_associative = function
  | { prel_desc = Associative s; prel_loc = _; } :: _ -> Some s
  | _ :: l -> get_associative l
  | [] -> None
;;

let rec get_absorbing = function
  | { prel_desc = Absorbing (s, g); prel_loc = _; } :: _ -> Some (s, g)
  | _ :: l -> get_absorbing l
  | [] -> None
;;

let rec get_absorbent = function
  | { prel_desc = Absorbent (s, g); prel_loc = _; } :: _ -> Some (s, g)
  | _ :: l -> get_absorbent l
  | [] -> None
;;

let rec get_neutral = function
  | { prel_desc = Neutral (s, g); prel_loc = _; } :: _ -> Some (s, g)
  | _ :: l -> get_neutral l
  | [] -> None
;;

let rec get_neutral_left = function
  | { prel_desc = Neutral ((Left | Both as s), g); prel_loc = _; } :: _ ->
    Some (s, g)
  | _ :: l -> get_neutral l
  | [] -> None
;;

let rec get_neutral_right = function
  | { prel_desc = Neutral ((Right | Both as s), g); prel_loc = _; } :: _ ->
    Some (s, g)
  | _ :: l -> get_neutral l
  | [] -> None
;;

let rec get_neutral_both = function
  | { prel_desc = Neutral (Both as s, g); prel_loc = _; } :: _ ->
    Some (s, g)
  | _ :: l -> get_neutral l
  | [] -> None
;;

let rec get_idempotent = function
  | { prel_desc = Idempotent s; prel_loc = _; } :: _ -> Some s
  | _ :: l -> get_idempotent l
  | [] -> None
;;

let rec get_nilpotent = function
  | { prel_desc = Nilpotent (s, g); prel_loc = _; } :: _ -> Some (s, g)
  | _ :: l -> get_nilpotent l
  | [] -> None
;;

let rec get_inverse = function
  | { prel_desc = Inverse (s, g, o); prel_loc = _; } :: _ -> Some (s, g, o)
  | _ :: l -> get_inverse l
  | [] -> None
;;

let commutative = get_opt_of_relations get_commutative
and associative = get_opt_of_relations get_associative
and absorbing = get_opt_of_relations get_absorbing
and absorbent = get_opt_of_relations get_absorbent
and neutral = get_opt_of_relations get_neutral
and neutral_right = get_opt_of_relations get_neutral_right
and neutral_left = get_opt_of_relations get_neutral_left
and neutral_both = get_opt_of_relations get_neutral_both
and idempotent = get_opt_of_relations get_idempotent
and nilpotent = get_opt_of_relations get_nilpotent
and inverse = get_opt_of_relations get_inverse
;;

let rec is_neutral_generator g =
  predicate_on_relations (is_neutral_with_generator g)

and is_neutral_with_generator g rel =
  match rel.prel_desc with
  | Neutral (_, neutr) -> g.pgen_desc = neutr.pgen_desc
  | _ -> false
;;

(** {6 Precedence on relations.} *)

(* The old simpler way:
let precedence rel =
  match rel.prel_desc with
  | Rewrite _ ->               10
  | Associative _ ->           20
  | Commutative _ ->           30
  | Division_by_Absorbent _
  | Neutral _
  | Involutive
  | Absorbent _
  | Absorbing _
  | Structure_item _
  | Status _
  | Precedence _ ->            40
  | Inverse _ ->               50
  | Nilpotent _
  | Idempotent _ ->            60
  | Distributive _ ->          70
;;
*)

let precedence rel =
  match rel.prel_desc with
  (* (0) Structural only rules *)
  | Rewrite _ ->               10
  (* neutral must be before inverse *)
  | Neutral _ ->                20
  | Absorbent _ ->              30
  | Division_by_Absorbent _ ->  40
  | Involutive ->               50

  (* (1) Rules that need equality *)
  | Nilpotent _ ->             100
  | Idempotent _ ->            110
  | Absorbing _ ->             120
  | Inverse _ ->               200

  | Associative _ ->           300

  (* (2) Rules that need comparison *)
  | Commutative _ ->           400

  (* Structural but should be done in last resort *)
  | Distributive _ ->          500

  (* (3) Not a rewrite rule: order is irrelevant *)
  (* For rpo during completion with KB *)
  | Precedence _
  | Status _ ->               1000
  (* Programmer definitions that are written as such
     before the construction function definitions.
     A convenient way to define a specific comparison
     that must be used in the construction functions. *)
  | Structure_item _ ->       2000
;;

(** {6 Comparison function on relations} *)

(** We define a comparison function on relations in the spirit of
    Pervasives.compare. We avoid to take into account locations,
    and use precedences of relations when appropriate. *)

let compare_generator g1 g2 =
  Pervasives.compare g1.pgen_desc g2.pgen_desc
(** We compare the name of generator, not their AST. *)
;;

let eq_generator g1 g2 = compare_generator g1 g2 = 0
;;

let compare_side s1 s2 =
  if s1 = Both or s2 = Both
  then 0
  else Pervasives.compare s1 s2
(** Both is equal to any other side. *)
;;

let eq_side s1 s2 = compare_side s1 s2 = 0
;;

(** To compare relations,

    Two non algebraic relations of the same kind, i.e. two Rewrite or
    Structure_items relations, are considered equal, whatever their contents
    can be.

    Commutative relations are considered equal whatever their comparison function
    companion could be.
    Associative relations are also considered equal whatever side they are
    carrying, given that the parser only generate the side Right.

    On any other case:

    - we first test the equality of relations,
    - otherwise, we use the precedence to decide the ordering,
      and in case of equal precedence, we use Pervasives.compare.

    So, we use the following specification:
    r1 = r2 iff
    - r1 & r2 are the same kind of relation &&
    - r1 & r2 have the same arguments (getting rid of locations).

    if not r1 = r2, then we compare their relative precedences:
    - if precedence of r1 = precedence of r2 then
    r1 and r2 are in their Pervasives.compare order.
    - else (precedence of r1 <> precedence of r2)
    r1 and r2 are in the order of their precedences.
*)

let compare_relation prec r1 r2 =
  match r1.prel_desc, r2.prel_desc with

  | Structure_item _, Structure_item _
  | Rewrite _, Rewrite _
  | Commutative _, Commutative _
  | Associative _, Associative _
  | Involutive, Involutive -> 0

  | Idempotent s1, Idempotent s2 -> compare_side s1 s2

  | Division_by_Absorbent g1, Division_by_Absorbent g2 ->
    compare_generator g1 g2

  | Nilpotent (s1, g1), Nilpotent (s2, g2)
  | Absorbent (s1, g1), Absorbent (s2, g2)
  | Absorbing (s1, g1), Absorbing (s2, g2)
  | Neutral (s1, g1), Neutral (s2, g2) ->
    begin
      match compare_generator g1 g2 with
      | 0 -> compare_side s1 s2
      | n -> n
    end

  | Distributive (s1, g1, _h1opt, _b1), Distributive (s2, g2, _h2opt, _b2) ->
    begin
      match compare_generator g1 g2 with
      | 0 -> compare_side s1 s2
      | n -> n
    end

  | Inverse (s1, g1, _), Inverse (s2, g2, _) ->
    begin
      match compare_generator g1 g2 with
      | 0 -> compare_side s1 s2
      | n  -> n
    end

  | (Precedence _ | Status _), _
  | _, (Precedence _ | Status _)
  | _, _ ->
    begin
      match Pervasives.compare (prec r1) (prec r2) with
      | 0 -> Pervasives.compare r1 r2
      | n -> n
    end
;;

let sort_rels prec = List.stable_sort (compare_relation prec)
;;

let sort_relations prec relations =
  match relations.prels_desc with
  | Prels_none
  | Prels_commented _ -> relations
  | Prels_begend rels ->
    {
      relations with
      prels_desc = Prels_begend (sort_rels prec rels);
    }
;;

module RelOrd = struct
  type t = relation;;
  let compare = compare_relation precedence;;
  let fprintf ppf _rel =
    Format.fprintf ppf "RelOrd.fprintf not yet implemented";;
end
;;

module RelMap = Map.Make (RelOrd);;
module RelSet = Myset.Make (RelOrd);;
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
