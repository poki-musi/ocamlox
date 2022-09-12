open Batteries
open Lexing

let span_of_pos p = (p.pos_lnum, p.pos_cnum - p.pos_bol)

let multispan_of_pos (p1, p2) =
  let x1, y1 = span_of_pos p1 in
  let x2, y2 = span_of_pos p2 in
  (x1, y1, x2, y2)


let pp_ast c f (a, loc) =
  c f a;
  let x1, y1, x2, y2 = multispan_of_pos loc in
  Fmt.(pf f "[%d:%d => %d:%d] " x1 y1 x2 y2)


type 'a ast = 'a * (position * position)

type node = node_t ast

and node_t =
  | SExpr of node
  | SVar of string * node option
  | SBlock of node list
  | SIf of node * node * node option
  | SWhile of node * node
  | SFun of fun_desc
  | SReturn of node option
  | SClass of string * fun_desc list * node option
  (* Ops *)
  | BinOp of binop * node * node
  | UniOp of uniop * node
  | App of node * node list
  (* Set/Get *)
  | Get of node * string
  | Set of node * string * node
  | Assign of node * node
  (* Literals *)
  | LId of string
  | LNum of float
  | LString of string
  | LTrue
  | LFalse
  | LNil
  | LThis of node
  | LSuper of node * string
  | Lookup of int option * string

and fun_desc = string * string list * node list

and binop =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpLE
  | OpLT
  | OpGE
  | OpGT
  | OpEQ
  | OpNEQ
  | OpAnd
  | OpOr

and uniop =
  | OpNeg
  | OpMinus
[@@deriving show { with_path = false }]

let build_for ((_, loc) as block) decl cond expr =
  let cond = cond |? (LTrue, loc) in
  let body =
    match expr with
    | Some expr ->
        (SBlock [ block; (SExpr expr, loc) ], loc)
    | None ->
        block
  in
  let for_body = SWhile (cond, body) in
  match decl with Some v -> SBlock [ v; (for_body, loc) ] | _ -> for_body
