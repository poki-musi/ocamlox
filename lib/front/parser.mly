%{
open Ast
open Batteries
%}

(* One Char Tokens *)

%token
  LPAREN RPAREN LBRACE RBRACE COMMA DOT
  MINUS PLUS SEMICOLON STAR SLASH

(* One/Two Char Tokens *)

%token
  BANG BANG_EQ EQ EQ_EQ GT GE LT LE

(* Keywords *)

%token
  AND CLASS ELSE FALSE FUN FOR IF NIL OR
  RETURN SUPER THIS TRUE VAR WHILE

(* Literals *)

%token <string> ID
%token <float> NUM
%token <string> STR

(* EOF *)

%token EOF

(* Starting Point *)

%start<node list> prog

%%

prog: declaration* EOF { $1 }

declaration:
  | declaration_t { $1, $loc }
  | statement { $1 }
%inline declaration_t:
  | var_decl SEMICOLON { $1 }
  | FUN ID fun_body { let a, b = $3 in SFun ($2, a, b) }
  | CLASS ID option(preceded(LT, variable)) LBRACE
      method_body*
    RBRACE { SClass ($2, $5, $3) }

var_decl: VAR ID ioption(preceded(EQ, expr)) { SVar ($2, $3) }

method_body:
  | ID fun_body { let params, body = $2 in ($1, params, body) }

statement: statement_t { $1, $loc }
%inline statement_t:
  | expr SEMICOLON { SExpr $1 }
  | RETURN option(expr) SEMICOLON { SReturn $2 }
  | IF LPAREN expr RPAREN statement { SIf ($3, $5, None) }
  | IF LPAREN expr RPAREN statement
        ELSE statement { SIf ($3, $5, Some $7) }
  | block { SBlock $1 }
  | WHILE LPAREN expr RPAREN statement { SWhile ($3, $5) }
  | FOR LPAREN
      option(for_decl)
      SEMICOLON
      option(expr)
      SEMICOLON
      option(expr)
    RPAREN statement { build_for $9 $3 $5 $7 }

for_decl:
  | var_decl { $1, $loc }
  | expr     { SExpr $1, $loc }

expr: assignment { $1 }

assignment:
  | variable EQ assignment { Assign ($1, $3), $loc($2) }
  | call DOT ID EQ assignment { Set ($1, $3, $5), $loc($2) }
  | logic_or { $1 }

logic_or:
  | logic_or OR equal { BinOp (OpOr, $1, $3), $loc($2) }
  | logic_and { $1 }

logic_and:
  | logic_and AND equal { BinOp (OpAnd, $1, $3), $loc($2) }
  | equal { $1 }

equal:
  | comp               { $1 }
  | equal EQ_EQ   comp { BinOp (OpEQ, $1, $3), $loc($2) }
  | equal BANG_EQ comp { BinOp (OpNEQ, $1, $3), $loc($2) }

comp:
  | term               { $1 }
  | comp LE term       { BinOp (OpLE, $1, $3), $loc($2) }
  | comp LT term       { BinOp (OpLT, $1, $3), $loc($2) }
  | comp GE term       { BinOp (OpGE, $1, $3), $loc($2) }
  | comp GT term       { BinOp (OpGT, $1, $3), $loc($2) }

term:
  | factor             { $1 }
  | term PLUS  factor  { BinOp (OpAdd, $1, $3), $loc($2) }
  | term MINUS factor  { BinOp (OpSub, $1, $3), $loc($2) }

factor:
  | unary              { $1 }
  | factor STAR unary  { BinOp (OpMul, $1, $3), $loc($2) }
  | factor SLASH unary { BinOp (OpDiv, $1, $3), $loc($2) }

unary:
  | unary_t            { $1, $loc }
  | call               { $1 }
%inline unary_t:
  | BANG unary         { UniOp (OpNeg, $2) }
  | MINUS unary        { UniOp (OpMinus, $2) }

call:
  | call_t             { $1, $loc }
  | lit                { $1 }
%inline call_t:
  | call LPAREN separated_list(COMMA, expr) RPAREN { App ($1, $3) }
  | call DOT ID { Get ($1, $3) }

lit:
  | lit_t              { $1, $loc }
  | variable           { $1 }
  | LPAREN expr RPAREN { $2 }
%inline lit_t:
  | NUM                { LNum $1 }
  | STR                { LString $1 }
  | TRUE               { LTrue }
  | FALSE              { LFalse }
  | NIL                { LNil }
  | THIS               { LThis (LId "this", $loc) }
  | SUPER DOT ID       { LSuper ((LId "super", $loc), $3) }

variable: ID { LId ($1), $loc }

fun_body:
  LPAREN separated_list(COMMA, ID) RPAREN block {
    ($2, $4)
  }

block: LBRACE declaration* RBRACE { $2 }
