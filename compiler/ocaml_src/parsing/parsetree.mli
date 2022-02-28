(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parsetree.mli,v 1.94 2012-01-31 09:12:58 bonichon Exp $ *)

(* Abstract syntax tree produced by parsing *)

(** {4 Modified OCaml parsetree for Moca} *)

(**/**)

open Asttypes

(* Type expressions for the core language *)

type core_type =
  { ptyp_desc: core_type_desc;
    ptyp_loc: Location.t }

and core_type_desc =
    Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t * core_type list
  | Ptyp_object of core_field_type list
  | Ptyp_class of Longident.t * core_type list * label list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * bool * label list option
  | Ptyp_poly of string list * core_type
  | Ptyp_package of package_type
  | Ptyp_parens of core_type

and package_type = Longident.t * (string * core_type) list

and core_field_type =
  { pfield_desc: core_field_desc;
    pfield_loc: Location.t }

and core_field_desc =
    Pfield of string * core_type
  | Pfield_var

and row_field =
    Rtag of label * bool * core_type list
  | Rinherit of core_type

(* Type expressions for the class language *)

type 'a class_infos =
  { pci_virt: virtual_flag;
    pci_params: string list * Location.t;
    pci_name: string;
    pci_expr: 'a;
    pci_variance: (bool * bool) list;
    pci_loc: Location.t }

(* Value expressions for the core language *)

type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }

and pattern_desc =
    Ppat_any
  | Ppat_var of string
  | Ppat_alias of pattern * string
  | Ppat_constant of constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t * pattern option * bool
  | Ppat_variant of label * pattern option
  | Ppat_record of (Longident.t * pattern) list * closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of Longident.t
  | Ppat_lazy of pattern
  | Ppat_parens of pattern

type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }

and expression_desc =
    Pexp_ident of Longident.t
  | Pexp_constant of constant
  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_function of label * expression option * (pattern * expression) list
  | Pexp_apply of expression * (label * expression) list
  | Pexp_match of expression * (pattern * expression) list
  | Pexp_try of expression * (pattern * expression) list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t * expression option * bool
  | Pexp_variant of label * expression option
  | Pexp_record of (Longident.t * expression) list * expression option
  | Pexp_field of expression * Longident.t
  | Pexp_setfield of expression * Longident.t * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of string * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type option * core_type option
  | Pexp_when of expression * expression
  | Pexp_send of expression * string
  | Pexp_new of Longident.t
  | Pexp_setinstvar of string * expression
  | Pexp_override of (string * expression) list
  | Pexp_letmodule of string * module_expr * expression
  | Pexp_assert of expression
  | Pexp_assertfalse
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string * expression
  | Pexp_pack of module_expr * package_type
  | Pexp_open of Longident.t * expression
  | Pexp_parens of expression
  | Pexp_begend of expression

(* Value descriptions *)

and value_description =
  { pval_type: core_type;
    pval_prim: string list }

(**/**)

(* Type declarations *)
(** {2 Type declarations with Moca additions } *)

and type_declaration =
  { ptype_params: string list;
    ptype_cstrs: (core_type * core_type * Location.t) list;
    ptype_kind: type_kind;
    ptype_private: private_flag;
    ptype_manifest: core_type option;
    ptype_variance: (bool * bool) list;
    ptype_loc: Location.t }

and type_kind =
  | Ptype_abstract of relations
  | Ptype_variant of
      (string * core_type list * relations * Location.t) list
  | Ptype_record of
      (string * mutable_flag * core_type * relations * Location.t) list

(** {2 Generators/relations for relational types } *)


(** {3 Definitions }

    {4 Definition: Relational data type}

         A {e relational data type} is a private data type with a set of
         relations that specifies the way the values of the type must be built.

    {4 Definition: Generator}

         The constructors of a relational data type are named {e
         generators}, to emphasis their special status of constructors with
         relations that govern their associated construction functions.

    {4 Definition: Arity of generators}

         A {e zeroary} generator is a generator that has no argument.
           A zeroary generator is also called a constant generator,

         A {e unary} generator is a generator that has exactly one
           argument {e which is not a list},

         A {e binary} generator is a generator that has exactly two
           arguments,

         A {e listary} generator is a generator that has exactly one
           argument which is a list.
           A listary generator generator is also called a {e vary-adic}
           generator.

         A {e nary} generator is a generator that has $n$ arguments
           ($n >= 3$).

    {4 Definition: Compatibility of arities}

         The arity $e$ is {e compatible} with arity $d$, if and only if
         $e$ is listary or $e = d$.

     Note: if a relation is defined for a listary generator and the relation
     states a particular case for singletons, then the relation {e must}
     also have a definition for unary generators and both definitions {e
     must} agree.

    {4 Definition: Comparison function}

         A {e comparison function} is a total ordering on the relational
         data type that is compatible with the structural equality of the
         language.

         In the usual mathematical sense, this definition expands to the
         following:

           [cmp] is a comparison function if and only if:

             - [cmp] induces a [ >= ] total ordering:

               Define by [ x >= y] if and only if [ cmp x y >= 0 ]; then
               [ >= ] must be a total ordering:
                 - [ >= ] is reflexive:
                     for all x, x [ >= ] x
                 - [ >= ] is transitive:
                     for all x y z, x [ >= ] y /\ y [ >= ] z => x [ >= ] z
                 - [ >= ] is anti-symmetric:
                     for all x y, x [ >= ] y /\ y [ >= ] x => x $=$ y
                     (in this definition $=$ is the syntactic equality, ie.
                      x $=$ y if and only if [ Pervasives.compare x y = 0 ])
                 - [ >= ] is total:
                     for all x y, x [ >= ] y \/ y [ >= ] x

             - [cmp ] is compatible with the language structural equality:
                 $\forall x y. cmp x y = 0 => Pervasives.compare x y = 0$.

       Note: This definition implies that a comparison function {e must} be a
       total function (it {e must} always terminates).

       The argument option of algebraic relation commutative is the name of a
       comparison function that is used to sort the arguments of the
       given generator. The comparison function is normally bound via a previous
       [Structure_item] definition of the relational data type definition.

       If the arguments of the commutative generator have type [ t ] then the
       comparison function must have type [ t -> t -> int ].

*)

and relation_side =
  | Left
  | Right
  | Both

and distributivity_direction =
  | Dist_Inverse
  | Dist_Direct

and rpo_status =
  | Lexicographic
  | Multiset

and rpo_precedence = int

and generator =
  { pgen_desc: Longident.t;
    pgen_loc: Location.t; }

and relations =
  { prels_desc: relations_desc;
    prels_loc: Location.t }

and relations_desc =
  | Prels_none
  | Prels_commented of relations
  | Prels_begend of relation list

and relation =
  { prel_desc: rel_desc;
    prel_loc: Location.t; }


(** {2 Specifications of algebraic relations for generators} *)

and rel_desc =
  | Absorbent of relation_side * generator
    (** {3 Specification for Absorbent (side, A)}

       - For zeroary generators: not applicable.

       - For unary generators:

         If C has Absorbent (Both, A), then
            C (A) -> A.

         Absorbent (Left, A) and Absorbent (Right, A) are erroneous.

       - For binary generators:

         If C has Absorbent (Left, A), then
            C (A, x) -> A.

         If C has Absorbent (Right, A), then
            C (x, A) -> A.

         Absorbent (Both, A) is the conjunction of
         Absorbent (Left, A) and Absorbent (Right, A).

       - For listary generators:

         If C has Absorbent (Both, A), then
            C [b1; ...; bn; A; c1; ...; cn] -> A.
         In particular:
            C [ A ] -> A.

         If C has Absorbent (Left, A), then
            C [b1; ...; bn; A; c1; ...; cn] -> C [b1; ...; bn; A].
            C [ A ] -> A.

         If C has Absorbent (Right, A), then
            C [b1; ...; bn; A; c1; ...; cn] -> C [A; c1; ...; cn].
            C [ A ] -> A.

         Absorbent (Both, A) is the conjunction of
         Absorbent (Left, A) and Absorbent (Right, A).

      - For nary generators: not applicable.

     Overall hint for absorbent:
         Absorbent (side, A) is (mostly) equivalent to
         Distributive (side, A, None, Dist_Direct).
    *)

    (* For Moca developpers:

      In the case of absorbent for nary generators,
      we could envision an additional parameter to specify which argument
      is absorbent, but this is not worth the burden. *)

  | Absorbing of relation_side * generator
    (** {3 Specification for Absorbing}

       - For zeroary generators: not applicable

       - For unary generators: not applicable

       - For binary generators:

         if C has Absorbing (Left, D), then
            C (D (x, y), y) -> y,

         if C has Absorbing (Right, D), then
            C (x, D (x, y)) -> x,

         Absorbing (Both, D) is the conjunction of
         Absorbing (Left, D) and Absorbing (Right, D).

       - For listary generators: not applicable.

       - For nary generators: not applicable.
    *)

    (* For Moca developpers:

       A temptative definition of Absorbing for listary generators, could
       be along the lines of:

         If C has Absorbing (Left, A), then
            C [b1; ...; bn; D [x; ...; y]; y; c1; ...; cn] ->
              C [b1; ...; bn; D [x; ...; y]; c1; ...; cn].

         If C has Absorbing (Right, A), then
          C [b1; ...; bn; x; D [x; ...; y]; c1; ...; cn] ->
            C [b1; ...; bn; x; c1; ...; cn]

         Absorbing (Both, D) is the conjunction of
         Absorbing (Left, D) and Absorbing (Right, D).
    *)

  | Associative of relation_side
    (** Note: for the time being the [relation_side] argument cannot be
       specified in source code.
       The default is [Left] (hence associative means Associative [Left]).

       {3 Specification for Associative}

       - For zeroary generators: not applicable.

       - For unary generators: not applicable.

       - For binary generators:

         If C has Associative (Left), then
            C (C (x, y), z) -> C (x, C (y, z)).

         If C has Associative (Right), then
            C (x, C (y, z)) -> C (C (x, y), z).

         Associative (Both) is equivalent to
         Associative (Left).

       - For listary generators:

         If C has Associative (Left), then (flattening)
            C [...; C [x; ...; y]; ...] -> C [...; x; ...; y; ...],

         Associative (Both) and Associative (Right) are equivalent to
         Associative (Left).

       - For nary generators: not applicable.
    *)

  | Commutative of Longident.t option
    (**
       {3 Specification for Commutative (cmp)}

       The optional argument [cmp] is a comparison function, as defined above.

       If no [cmp] option is given, the default comparison function is [
       Pervasives.compare ].
       In other words, the relation Commutative (None) is equivalent to the
       relation Commutative (Some Pervasives.compare).

       - For zeroary generators: not applicable.

       - For unary generators: not applicable.

       - For binary generators:

           If C is not associative:

             If C has Commutative (Some cmp), then the arguments of
             C are sorted in increasing order with respect to [cmp],

               C (x, y) -> C (y, x) if cmp x y > 0.

           If C is associative:

             If C has Commutative (Some cmp), then the leaves of
             C-combs are sorted in increasing order with respect to [cmp],

               C (x, C (y, z)) -> C (y, C (x, z)) if cmp x y > 0.
               C (x, y) -> C (y, x) if cmp x y > 0
                                    and y not of the form C (u, v).

       - For listary generators:

         If C has Commutative (Some cmp), then the elements of the list
         argument of C are sorted in increasing order with respect to [cmp],

               C [...; x; y; ...] -> C [...; y; x; ...] if cmp x y > 0.

       - For nary generators: not applicable.
    *)

  | Distributive of relation_side * generator *
                    generator option * distributivity_direction
    (**
       {3 Prerequisites for distributivity}

       - For having Distributive (side, D, Some E, dir),
       the arity of E must be compatible with the arity of D
       (compatibility of arities is define above)..

       - Distributive (side, D, None, dir) is equivalent to
       Distributive (side, D, Some D, dir).

       - In relation Distributive (side, D, Some E, dir), the [dir] argument
       governs the order of the arguments of generator E in the right-hand
       side of the following definition rules for distributivity.

       - In particular, to obtain the rules for
              Distributive (side, D, Some E, Dist_Inverse),
       take the rules for
              Distributive (side, D, Some E, Dist_Direct)
       and simply write the arguments of E in reverse order.

       {3 Specification for
         Distributive (side, generator, generator option, dir)}

       - For zeroary generators: not applicable.

       - For unary generators:

         If C has Distributive (Both, D, None, dir), then
          C has Distributive (Both, D, Some D, dir), else

         If C has Distributive (Both, D, Some E, dir), then
           if E is zeroary (then D must be zeroary),
            C (D) -> E

           if E is unary (then D must be unary),
            C (D (x)) -> E (x)

           if E is binary (then D must be binary),
            if dir = Dist_Direct then
             C (D (x, y)) -> E (x, y) else
             C (D (x, y)) -> E (y, x)

           if E is listary then
             if D is:
             - zeroary
               C (D) -> E []
             - unary
               C (D (y)) -> E [C (y)]
             - binary
               C (D (y1, y2)) -> E [C (y1); C (y2)]
             - listary
               C (D [y1; ...; yn]) -> E [C (y1); ...; C (yn)]
             - nary
               C (D (y1; ...; yn)) -> E [C (y1); ...; C (yn)]

           if E is nary (then D must be nary),
               C (D (y1; ...; yn)) -> E (C (y1); ...; C (yn))

         Distributive (Right, D, generator_option, dir) and
         Distributive (Left, D, generator_option, dir) are errors.

       - For binary generators:

         If C has Distributive (Left, D, Some E, Dist_Direct), then
            if E is zeroary (then D must be zeroary),
               C (D, z) -> E,
            if E is unary (then D must be unary),
               C (D (x), z) -> E (C (x, z)),
            if E is binary (then D must be binary),
               C (D (x, y), z) -> E (C (x, z), C (y, z)),
            if E is listary:
               if D is zeroary:
                  C (D, z) -> E [],
               if D is unary:
                  C (D (x), z) -> E [C (x, z)],
               if D is binary:
                  C (D (x, y), z) -> E [C (x, z); C (y, z)],
               if D is listary:
                  C (D [x1, ..., xn], z) -> E [C (x1; z), ..., C (xn; z)],
               if D is nary:
                  C (D (x1, ..., xn), z) -> E [C (x1, z), ..., C (xn, z)],
            if E is nary (then D must be nary),
               C (D (x1, ..., xn), z) -> E (C (x1, z), ..., C (xn, z)),

         If C has Distributive (Right, D, Some E, Dist_Direct), then
            C (z, D) -> E if D and E are zeroary,
            C (z, D (x1, ..., xn)) -> E (C (z, x1), ..., C (z, xn)) otherwise.

         Distributive (Both, D, Some E, dir) is the conjunction of
         Distributive (Left, D, Some E, dir) and
         Distributive (Right, D, Some E, dir).

       - For listary generators:

         If C has Distributive (Both, D, Some E, Dist_Direct), then
            if E is zeroary (then D must be zeroary),
               C [u1; ...; un; D; t1; ...; tn ] -> E,
            if E is unary (then D must be unary),
               C [u1; ...; un; D (x); t1; ...; tn] ->
               E (C [u1; ...; un; x; t1; ...; tn]),

            if E is binary (then D must be binary),
               C [u1; ...; un; D (x, y); t1; ...; tn] ->
               E (C [u1; ...; un; x; t1; ...; tn],
                  C [u1; ...; un; y; t1; ...; tn]),

            if E is listary:
               if D is zeroary:
                  C [u1; ...; un; D; t1; ...; tn] -> E [],
               if D is unary:
                  C [u1; ...; un; D (x); t1; ...; tn] ->
                    E [C [u1; ...; un; x; t1; ...; tn]],
               if D is binary:
                  C [u1; ...; un; D (x, y); t1; ...; tn] ->
                    E [C [u1; ...; un; x; t1; ...; tn];
                       C [u1; ...; un; y; t1; ...; tn];
                      ]
               if D is listary:
                  C [u1; ...; un; D [x1; ...; xn]; t1; ...; tn] ->
                    E [C [u1; ...; un; x1; t1; ...; tn];
                       ...;
                       C [u1; ...; un; xn; t1; ...; tn];
                      ]
               if D is nary:
                  C [u1; ...; un; D (x1, ..., xn); t1; ...; tn] ->
                    E [C [u1; ...; un; x1; t1; ...; tn];
                       ...;
                       C [u1; ...; un; xn; t1; ...; tn];
                      ]

            if E is nary (then D must be nary with the same arity as E),
              C [u1; ...; un; D (x1, ..., xn); t1; ...; tn] ->
               E (C [u1; ...; un; x1; t1; ...; tn],
                  ...,
                  C [u1; ...; un; xn; t1; ...; tn])

         Distributive (Right, D, generator_option, dir) and
         Distributive (Left, D, generator_option, dir) are errors.

       - For nary generators: not applicable.
    *)

  | Division_by_Absorbent of generator
    (** This is an internal generator for Moca that is automatically added to
       the relations of a generator when appropriate.

       {3 Specification for Division_by_Absorbent (generator)}

       - For zeroary generators: not applicable.

       - For unary operators:

         If C has Division_by_Absorbent (A), then
            C (A) -> failwith "Division by absorbent element".

       - For binary generators:
         If C has Division_by_Absorbent (A), then
            C (x, A) -> failwith "Division by absorbent element".

       - For listary generators and
       - For nary generators:
         Division_by_Absorbent is not applicable.
    *)

  | Idempotent of relation_side
    (** {3 Specification of Idempotent (side)}

       - For zeroary generators: not applicable.

       - For unary generators:

         If C has Idempotent (Both), then
            C (C x) -> C x,

         Idempotent (Left) and Idempotent (Right) are errors.

       - For binary generators:

         If C has Idempotent (Left), then
            C (C (x, y), y) -> C (x, y),
            if x and C (x, x) have the same type, then
            C (x, x) -> x,

         If C has Idempotent (Right), then
            C (x, C (x, y)) -> C (x, y),
            if x and C (x, x) have the same type, then
            C (x, x) -> x,

         Idempotent (Both) is the conjunction of
         Idempotent (Left) and Idempotent (Right).

       - For listary generators:

         If C has Idempotent (Left), then
            C [...; x; x; ...] -> C [...; x; ...] if
              the list of arguments has >= 3 elements
            if x and C [x, x] have the same type, then
            C [x; x] -> x,

         Idempotent (Both) and Idempotent (Right) are equivalent to
         Idempotent (Left).

       - For nary generators: not applicable.
    *)

  | Inverse of relation_side * generator * generator option
    (** {3 Specification for Inverse (side, generator, generator option)}

       - For zeroary generators: not applicable.

       - For unary generators:

         If C has (Inverse (Left, I, None), then
            C (I (x)) -> x.

         If C has (Inverse (Right, I, None), then
            I (C (x)) -> x.

         Inverse (Both, I, None) is the conjunction of
         Inverse (Left, I, None) and Inverse (Right, I, None).

         If C has Inverse (Left, I, Some A), then
            C (I (x)) -> A.

         If C has Inverse (Right, I, Some A), then
            I (C (x)) -> A.

         Inverse (Both, I, Some A) is equivalent to
         Inverse (Left, I, Some A) and
         Inverse (Right, I, Some A).

       - For binary generators:

         If C has Inverse (side, I, None), then
            C must have Neutral (side, E) and
            Inverse (side, I, None) is equivalent to
            Inverse (side, I, Some E).

         If C has Inverse (Left, I, Some A), then
            I implicitely has Involutive and
            C implicitely has
              Distributive (Left, E, Some A, Dist_Direct) and

            C (I (x), x) -> A.

         If C has Inverse (Right, I, Some A), then
            I implicitely has Involutive and
            C implicitely has
              Distributive (Right, E, Some A, Dist_Direct) and

            C (x, I (x)) -> A

         Inverse (Both, I, Some A) is equivalent to
         Inverse (Left, I, Some A) and Inverse (Right, I, Some A).

       - For listary generators:

         If C has Inverse (side, I, None), then
            C must have Neutral (side, E) and
            Inverse (side, I, None) is equivalent to
            Inverse (side, I, Some E).

         If C has Inverse (Left, I, Some A), then
            I implicitely has Involutive and
            C implicitely has
            Distributive (Left, E, Some A, Dist_Direct) and

            I implicitely has Involutive and
            C implicitely has
            Distributive (Right, E, Some A, Dist_Direct) and

            C [...; I (x); x; ...] -> C [...; A; ...] if
              the list of arguments has >= 3 elements,
            C [I (x); x] -> A.

         If C has Inverse (Right, I, Some A), then
            I implicitely has Involutive and
            C implicitely has
            Distributive (Right, E, Some A, Dist_Direct) and

            C [...; x; I (x); ...] -> C [...; A; ...] if
              the list of arguments has >= 3 elements,
            C [x; I (x)] -> A.

         Inverse (Both, I, Some A) is the conjunction of
         Inverse (Left, I, Some A) and Inverse (Right, I, Some A).

       - For nary generators: not applicable.
    *)

  | Involutive
    (** {3 Specification for Involutive}

       - For zeroary generators: not applicable.

       - For unary generators:

         If C has Involutive, then
            C (C (x)) -> x.

       - For binary generators: not applicable.

       - For listary generators: not applicable.

       - For nary generators: not applicable.
    *)

  | Neutral of relation_side * generator
    (** {3 Specification for Neutral (side, generator)}

       - For zeroary generators: not applicable.

       - For unary generators:

         C has Neutral (side, E) is equivalent to E is a fixpoint for C.

         If C has Neutral (Both, E), then
            C (E) -> E.

         Neutral (Right, E) and Neutral (Left, E) are errors.

       - For binary generators:

         If C has Neutral (Left, E), then
            C (E, x) -> x,

         If C has Neutral (Right, E), then
            C (x, E) -> x,

         Neutral (Both, E) is the conjunction of
         Neutral (Left, E) and Neutral (Right, E).

       - For listary generators:

         If C has Neutral (Left, E), then
            C [...; E; x; ...] -> C [...; x; ...] if
              the list of arguments has at least 2 elements,
            C [x] -> x,
            C [] -> E,

         If C has Neutral (Right, E), then
            C [...; x; E; ...] -> C [...; x; ...] if
              the list of arguments has at least 2 elements,
            C [x] -> x
            C [] -> E,

         Neutral (Both, E) is the conjunction of
         Neutral (Left, E) and Neutral (Right, E).

       - For nary generators: not applicable.
    *)

  | Nilpotent of relation_side * generator
    (** {3 Specification for Nilpotent (side, generator)}

       - For zeroary generators: not applicable.

       - For unary generators:

         If C has Nilpotent (Both, A), then
            C (C x) -> A.

         Nilpotent (Left, A) and Nilpotent (Right, A) are errors.

       - For binary generators:

         If C has Nilpotent (Left, A), then
            C (C (x, y), y) -> C (x, A),
            C (x, x) -> A,

         If C has Nilpotent (Right, A), then
            C (x, C (x, y)) -> C (A, y),
            C (x, x) -> A,

         Nilpotent (Both, A) is the conjunction of
         Nilpotent (Left, A) and Nilpotent (Right, A).

       - For listary generators:

         If C has Nilpotent (Both, A), then

            C [...; x; x; ...] -> C [...; A; ...] if
              the list has >= 3 elements,
            C [x; x] -> A.

         Nilpotent (Left, A) and Nilpotent (Right, A) are equivalent to Both.

       - For nary generators: not applicable.
    *)

  | Precedence of rpo_precedence
    (** {5 Definition: Recursive Path Ordering}

       The {e Recursive Path Ordering} (rpo for short) is an ordering for
       terms.

       {3 Specification for Precedece p}

       Moca uses a rpo-based completion algorithm to complete the set of
       rules given by the programmer for the relational data type at hand.

       For any generator C, if C has Precedence (p) then C will have
       precedence p for the rpo-based completion.

       Note: integer p must be positive and not equal to [Pervasives.max_int].

       Per se, the Precedence annotation does not define any statically known
       rewrite rule. The annotation simply helps moca to complete the set of
       rules defined so far for the relational data type.
    *)

  | Rewrite of pattern * expression
    (** {3 Specification for Rewrite (pattern, expression)}

       - For any generator C,
         if C has relation Rewrite (pat, expr) then the
         first clause of its construction function is

           | pat -> expr

       This clause precedes any other clause necessary for non Rewrite
       algebraic relations.

       - Multiple Rewrite rules for generator C appears in C construction
       function in the order of presentation in the relational data type
       definition.
    *)

  | Status of rpo_status
    (** The Status relation is an annotation for a companion generator
       that mocac uses to complete the set of rules defined so far for the
       relational data type (Knuth-Bendix competion algorithm).

       Per se, the Status relation does not define any rewrite rule.

       {3 Specification of Status (stat)}

       - For any generator C, if C has property Status (stat) then C will
         have the stat status for the rpo-based completion.

      The status could be [lexicographic] or [multiset].
    *)

  | Structure_item of structure_item
    (** A [Structure_item] holds any Caml definition of some value associated
       to the relational type specification. Functions defined that way can
       be used inside the generated construction functions.
       The definition of a specific [compare] function for the values of the
       relational type is typical, to get a semantically sound comparison
       within the compiler generated construction functions.

      {3 Specification of Structure_item (str_item)}

      - For any generator C, if C has Structure_item (auxiliary_definition)
        then the definition auxiliary_definition) is written in the resulting
        implementation file {e before} the mocac generated definitions for
        construction functions.

        For instance, if C has annotation:
           begin
             let f = e;;
           end
        then the code "let f = e;;" will be written in the target file before
        any other mocac generated code.
    *)

(**/**)

(* Exception declarations *)

and exception_declaration = core_type list

(* Type expressions for the class language *)

and class_type =
  { pcty_desc: class_type_desc;
    pcty_loc: Location.t }

and class_type_desc =
    Pcty_constr of Longident.t * core_type list
  | Pcty_signature of class_signature
  | Pcty_fun of label * core_type * class_type

and class_signature = core_type * class_type_field list

and class_type_field =
    Pctf_inher of class_type
  | Pctf_val of (string * mutable_flag * virtual_flag * core_type * Location.t)
  | Pctf_virt  of (string * private_flag * core_type * Location.t)
  | Pctf_meth  of (string * private_flag * core_type * Location.t)
  | Pctf_cstr  of (core_type * core_type * Location.t)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(* Value expressions for the class language *)

and class_expr =
  { pcl_desc: class_expr_desc;
    pcl_loc: Location.t }

and class_expr_desc =
    Pcl_constr of Longident.t * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of label * expression option * pattern * class_expr
  | Pcl_apply of class_expr * (label * expression) list
  | Pcl_let of rec_flag * (pattern * expression) list * class_expr
  | Pcl_constraint of class_expr * class_type
  | Pcl_parens of class_expr

and class_structure = pattern * class_field list

and class_field =
    Pcf_inher of override_flag * class_expr * string option
  | Pcf_valvirt of (string * mutable_flag * core_type * Location.t)
  | Pcf_val of
      (string * mutable_flag * override_flag * expression * Location.t)
  | Pcf_virt of (string * private_flag * core_type * Location.t)
  | Pcf_meth of
      (string * private_flag * override_flag * expression * Location.t)
  | Pcf_cstr of (core_type * core_type * Location.t)
  | Pcf_let of rec_flag * (pattern * expression) list * Location.t
  | Pcf_init of expression

and class_declaration = class_expr class_infos

(* Type expressions for the module language *)

and module_type =
  { pmty_desc: module_type_desc;
    pmty_loc: Location.t }

and module_type_desc =
    Pmty_ident of Longident.t
  | Pmty_signature of signature
  | Pmty_functor of string * module_type * module_type
  | Pmty_with of module_type * (Longident.t * with_constraint) list
  | Pmty_typeof of module_expr
  | Pmty_parens of module_type

and signature = signature_item list

and signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }

and signature_item_desc =
    Psig_value of string * value_description
  | Psig_type of (string * type_declaration) list
  | Psig_exception of string * exception_declaration
  | Psig_module of string * module_type
  | Psig_recmodule of (string * module_type) list
  | Psig_modtype of string * modtype_declaration
  | Psig_open of Longident.t
  | Psig_include of module_type
  | Psig_class of class_description list
  | Psig_class_type of class_type_declaration list

and modtype_declaration =
    Pmodtype_abstract
  | Pmodtype_manifest of module_type

and with_constraint =
    Pwith_type of type_declaration
  | Pwith_module of Longident.t
  | Pwith_typesubst of type_declaration
  | Pwith_modsubst of Longident.t

(* Value expressions for the module language *)

and module_expr =
  { pmod_desc: module_expr_desc;
    pmod_loc: Location.t }

and module_expr_desc =
    Pmod_ident of Longident.t
  | Pmod_structure of structure
  | Pmod_functor of string * module_type * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression * package_type
  | Pmod_parens of module_expr

and structure = structure_item list

and structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_eval of expression
  | Pstr_value of rec_flag * (pattern * expression) list
  | Pstr_primitive of string * value_description
  | Pstr_type of (string * type_declaration) list
  | Pstr_exception of string * exception_declaration
  | Pstr_exn_rebind of string * Longident.t
  | Pstr_module of string * module_expr
  | Pstr_recmodule of (string * module_type * module_expr) list
  | Pstr_modtype of string * module_type
  | Pstr_open of Longident.t
  | Pstr_class of class_declaration list
  | Pstr_class_type of class_type_declaration list
  | Pstr_include of module_expr

(* Toplevel phrases *)

type toplevel_phrase =
    Ptop_def of structure
  | Ptop_dir of string * directive_argument

and directive_argument =
    Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
  | Pdir_bool of bool

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
