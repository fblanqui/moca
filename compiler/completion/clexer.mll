(**********************************************************************)
(*                                                                    *)
(*                           Moca                                     *)
(*                                                                    *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*          Frédéric Blanqui, projet Protheo, INRIA Lorraine          *)
(*                                                                    *)
(*  Copyright 2005-2008,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the Q Public License. *)
(*                                                                    *)
(**********************************************************************)

(* lexer *)

{
  open Cparser;;
}

let space = [' ' '\t' '\n']
let num = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let symbol = ['^' '#' '+' '-' '.' '*' '!' '~' '@' '%' '&' '|' '/' '>' '<' '?' '=' '\\' '$']
let vname = (lower num*)
let fname = (upper lower* num* | symbol+ num* | num | lower lower+ num*)

rule next_token = parse
  | eof { EOF }
  | space { next_token lexbuf }
  | '(' { LPAR }
  | ')' { RPAR }
  | "->" { ARROW }
  | ';' { SEMI_COLON }
  | '=' { EQ }
  | vname { VNAME (Lexing.lexeme lexbuf) }
  | fname { FNAME (Lexing.lexeme lexbuf) }
  | _ { BAD_CHAR }
(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
