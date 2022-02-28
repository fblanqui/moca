/**********************************************************************/
/*                                                                    */
/*                           Moca                                     */
/*                                                                    */
/*          Pierre Weis, INRIA Rocquencourt                           */
/*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          */
/*                                                                    */
/*  Copyright 2005-2008,                                              */
/*  Institut National de Recherche en Informatique et en Automatique. */
/*  All rights reserved.                                              */
/*                                                                    */
/*  This file is distributed under the terms of the Q Public License. */
/*                                                                    */
/**********************************************************************/

/* parser */

%{
  open Term;;
  open Symb;;
  open Var;;
  open Rule;;
  open Equation;;
%}

%token EOF
%token LPAR RPAR
%token ARROW
%token <string> VNAME
%token <string> FNAME
%token BAD_CHAR
%token EQ
%token SEMI_COLON

%start term top_term rule rules equation equations
%type <Term.term> term
%type <Term.term> top_term
%type <Rule.rule> rule
%type <Rule.RulSet.t> rules
%type <Equation.eqn> equation
%type <Equation.EqnSet.t> equations

%%

equations:
  | /* empty */                   { EqnSet.empty }
  | equation                      { EqnSet.singleton $1 }
  | equation SEMI_COLON equations { EqnSet.add $1 $3 }
;

equation:
  | top_term EQ top_term { Equation.mk ($1, $3) }
;

rules:
  | /* empty */            { RulSet.empty }
  | rule                   { RulSet.singleton $1 }
  | rule SEMI_COLON rules  { RulSet.add $1 $3 }
;

rule:
  | top_term ARROW top_term { Rule.mk ($1, $3) }
;

top_term:
  | term        { $1 }
  | FNAME terms { App (symbol_of_string $1, $2) }
;

term:
  | name                  { $1 }
  | LPAR FNAME terms RPAR { App (symbol_of_string $2, $3) }
;

name:
  | VNAME { Var (var_of_string $1) }
  | FNAME { App (symbol_of_string $1, []) }
;

terms:
  | term       { [ $1 ] }
  | term terms { $1 :: $2 }
;

%%
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
