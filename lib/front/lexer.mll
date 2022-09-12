{
  open Batteries
  open Lexing

  open Parser

  exception Error of string
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let whitespace = [' ' '\t']+
let str_t = '"'  '"'
let eol = ['\r' '\n'] | '\r' '\n'

rule token = parse
  | "/*"            { comment 0 lexbuf }
  | eof             { EOF }
  | eol             { new_line lexbuf; token lexbuf }
  | whitespace      { token lexbuf }
  | "//" [^ '\n']*  { token lexbuf }

  (* One Char Tokens *)
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | ','             { COMMA }
  | '.'             { DOT }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { STAR }
  | '/'             { SLASH }
  | ';'             { SEMICOLON }
  | "!="            { BANG_EQ }
  | '!'             { BANG }
  | "=="            { EQ_EQ }
  | '='             { EQ }
  | ">="            { GE }
  | '>'             { GT }
  | "<="            { LE }
  | '<'             { LT }

  (* Keywords *)
  | "and"           { AND }
  | "true"          { TRUE }
  | "class"         { CLASS }
  | "else"          { ELSE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "for"           { FOR }
  | "if"            { IF }
  | "nil"           { NIL }
  | "or"            { OR }
  | "return"        { RETURN }
  | "super"         { SUPER }
  | "this"          { THIS }
  | "var"           { VAR }
  | "while"         { WHILE }

  (* Literals *)
  | id as s                             { ID s }
  | '"' (([^ '"'] | "\\\"" )* as s)     { str_tail lexbuf; STR s }
  | (digit+ ('.' digit*)?) as s         { NUM (float_of_string s) }

  (* Error *)
  | _ as s          { raise (Error ("unknown character '"^(String.make 1 s)^"'")) }

and str_tail = parse
  | '"' { () }
  | eof { raise (Error ("non-terminated string"))  }

and comment level = parse
  | "*/" {if level = 0 then token lexbuf
          else comment (level-1) lexbuf}
  | "/*" {comment (level+1) lexbuf}
  | _    {comment level lexbuf}
