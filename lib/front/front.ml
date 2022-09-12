open Batteries
open Parser
module Parser = Parser
module Ast = Ast

exception RuntimeErr
exception CompileErr

let str l p msg =
  Fmt.(str "error: %i:%i %s@." l (p + 1) msg)

let str_loc (loc, _) msg =
  let l, p = Ast.span_of_pos loc in
  str l p msg

let fmt ppf l p msg =
  Fmt.(pf ppf "error: %i:%i %s@." l (p + 1) msg)

let fmt_loc ppf (loc, _) msg =
  let l, p = Ast.span_of_pos loc in
  fmt ppf l p msg

let runtime l p msg =
  fmt Fmt.stderr l p msg;
  raise RuntimeErr

let parse ch =
  let lexbuf = Lexing.from_channel ch in
  let show_err msg =
    let l, p = Ast.span_of_pos lexbuf.lex_curr_p in
    Fmt.(pf stderr "error: %i:%i: %s\n@?" l p msg)
  in
  try Some (Parser.prog Lexer.token lexbuf)
  with err ->
    (match err with
    | Lexer.Error msg ->
        show_err msg
    | Parser.Error ->
        show_err "grammar error"
    | _ ->
        ());
    None

let[@ocamlformat "disable"] string_of_token = function
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | COMMA -> ","
  | DOT -> "."
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | SLASH -> "/"
  | SEMICOLON -> ";"
  | BANG_EQ -> "!="
  | BANG -> "!"
  | EQ_EQ -> "=="
  | EQ -> "="
  | GE -> ">="
  | GT -> ">"
  | LE -> "<="
  | LT -> "<"
  | AND -> "and"
  | TRUE -> "true"
  | CLASS -> "class"
  | ELSE -> "else"
  | FALSE -> "false"
  | FUN -> "fun"
  | FOR -> "for"
  | IF -> "if"
  | NIL -> "nil"
  | OR -> "or"
  | RETURN -> "return"
  | SUPER -> "super"
  | THIS -> "this"
  | VAR -> "var"
  | WHILE -> "while"
  | ID s -> "id<" ^ s ^ ">"
  | STR s -> "\"" ^ s ^ "\""
  | NUM n -> string_of_float n
  | EOF -> "EOF"
