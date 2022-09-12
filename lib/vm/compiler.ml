open Batteries
include Common
module Op = Intr.Op
module V = Intr.V

type t = {
  code : char Vect.t;
  constants : V.t Vect.t;
  positions : (int * int) Vect.t;
  locals : string list;
  level : int list;
  error : bool;
  decl_var : string;
  enclosing : t option;
  fn_arity : int;
  fn_name : string;
  fn_type : [ `Script | `Function | `Method | `Init ];
  cls_type : [ `Script | `Class | `Super ];
  upvalues : (string * int * bool) list;
}

(** Conversions/Creation *)

let with_var name cmp = { cmp with decl_var = name }

let make
    ?(fn_type = `Script)
    ?(cls_type = `Script)
    ?(fn_name = "script")
    ?(fn_arity = 0)
    () =
  {
    code = Vect.empty;
    constants = Vect.empty;
    positions = Vect.empty;
    error = false;
    locals = [ "" ];
    level = [];
    decl_var = "";
    fn_name;
    fn_type;
    fn_arity;
    cls_type;
    upvalues = [];
    enclosing = None;
  }

let to_chunk b : Intr.Chunk.t =
  {
    code = b.code |> Vect.enum |> Buffer.of_enum |> Buffer.to_bytes;
    constants = b.constants |> Vect.to_array;
    lines = b.positions |> Vect.to_array;
  }

let to_fun b : Intr.chunk Intr.fun_body_t =
  { sign = b.fn_name; arity = b.fn_arity; body = to_chunk b }

let to_closure b : V.t =
  FunClos ({ sign = b.fn_name; arity = b.fn_arity; body = to_chunk b }, [||])

(** Error management *)

let fail loc msg cmp =
  Fmt.(pf stderr "%s@." @@ Front.str_loc loc msg);
  { cmp with error = true }

(** Pushing data into the builder *)

let byte c cmp =
  {
    cmp with
    code = Vect.append c cmp.code;
    positions = Vect.append (0, 0) cmp.positions;
  }

let op op (loc, _) cmp =
  {
    cmp with
    code = Vect.append Op.(to_byte op) cmp.code;
    positions = Vect.append (Front.Ast.span_of_pos loc) cmp.positions;
  }

let add_value v cmp =
  let addr = Vect.length cmp.constants in
  let cmp = { cmp with constants = Vect.append v cmp.constants } in

  if addr <= 255
  then cmp |> byte (char_of_int addr)
  else
    let lo, hi = bytes_of_int16 addr in
    cmp |> byte lo |> byte hi

let constant v loc cmp =
  let addr = Vect.length cmp.constants in

  let open Op in
  if addr <= 255
  then cmp |> op CONST loc |> add_value v
  else cmp |> op LONG_CONST loc |> add_value v

let emit_jump opcode loc cmp =
  let addr = Vect.length cmp.code + 1 in
  (cmp |> op opcode loc |> byte '0' |> byte '0', addr)

let patch_jump jump cmp =
  let addr = Vect.length cmp.code in
  let hi, lo = (Char.chr (addr / 256), Char.chr (addr mod 256)) in
  { cmp with code = Vect.set (Vect.set cmp.code jump lo) (jump + 1) hi }

let emit_loop addr opcode loc cmp =
  let hi, lo = (Char.chr (addr / 256), Char.chr (addr mod 256)) in
  cmp |> op opcode loc |> byte lo |> byte hi

(** Local variables *)

let rec nest cmp loc f = cmp |> push_ctx |> f |> pop_ctx loc
and push_ctx cmp = { cmp with level = 0 :: cmp.level }

and pop_ctx loc cmp =
  match cmp.level with
  | n :: _ ->
      let cmp' = apply_n (op POP loc) cmp n in
      { cmp' with level = List.tl cmp.level; locals = List.drop n cmp'.locals }
  | _ ->
      cmp

let rec declare_var name loc cmp =
  match [@ocamlformat "disable"] cmp.level with
  | [] ->
      cmp
  | level :: tail ->
      if  List.length cmp.locals > 256 then
        fail loc "cannot have more than 256 locals" cmp
      else if cmp.locals |> List.take level |> List.mem name then
        fail loc ("cannot define '" ^ name ^ "' twice") cmp
      else
        { cmp with locals = name :: cmp.locals; level = (level + 1) :: tail }

and define_var name loc cmp =
  match cmp.level with
  | [] ->
      cmp |> constant (String name) loc |> op Op.DEF_GLOBAL loc
  | _ :: _ ->
      cmp

and resolve_variable name cmp =
  match resolve_local name cmp with
  | Some idx ->
      (`FoundLocal idx, cmp)
  | None ->
      resolve_upvalue name cmp

and resolve_upvalue name cmp =
  match cmp.enclosing with
  | None ->
      (`NoLocals, cmp)
  | Some cmp' -> (
    match resolve_local name cmp' with
    | Some idx ->
        let idx', cmp' = add_upvalue false name idx cmp in
        (`FoundUpvalue idx', { cmp with enclosing = Some cmp' })
    | None -> (
        let found, cmp' = resolve_upvalue name cmp' in
        match found with
        | `FoundLocal idx | `FoundUpvalue idx ->
            let idx', cmp'' = add_upvalue false name idx cmp' in
            (`FoundUpvalue idx', { cmp with enclosing = Some cmp'' })
        | `NoLocals ->
            (`NoLocals, cmp)))

and add_upvalue is_local name value_idx cmp =
  match
    List.drop_while
      (fun (name', _, is_local') -> name <> name' || is_local <> is_local')
      cmp.upvalues
  with
  | [] ->
      let idx = List.length cmp.upvalues in
      (idx, { cmp with upvalues = (name, value_idx, is_local) :: cmp.upvalues })
  | xs ->
      (List.length xs - 1, cmp)

and resolve_local name cmp =
  match
    cmp.locals |> Seq.of_list |> Seq.drop_while (( <> ) name) |> Seq.length
  with
  | 0 ->
      None
  | n ->
      Some (n - 1)

(** Utilities *)

let maybe_do f ast cmp = Option.map_default (fun ast -> f ast cmp) cmp ast

let maybe_do_else
    (f : 'a -> 'b -> 'b)
    (g : 'b -> 'b)
    (ast : 'a option)
    (cmp : 'b) =
  match ast with Some v -> f v cmp | None -> g cmp

(** Main functions *)

let rec main loc stmts = compile_body loc stmts @@ make ()

and compile_body loc stmts cmp =
  let cmp = cmp |> compile_list stmts |> op NIL loc |> op Op.RET loc in
  if cmp.error then None else Some (to_closure cmp, cmp)

and compile_list stmts cmp = List.fold_left (flip compile) cmp stmts

and compile (ast, loc) cmp =
  let open Front.Ast in
  match ast with
  | SExpr expr ->
      cmp |> compile expr |> op Op.POP loc
  | SVar (name, expr) ->
      cmp
      |> declare_var name loc
      |> (match expr with
         | Some expr ->
             with_var name %> compile expr %> with_var ""
         | None ->
             op NIL loc)
      |> define_var name loc
  | SIf (cond, ((_, loc_then) as then_), else_) ->
      let cmp, jump_if =
        cmp |> compile cond |> emit_jump Op.JUMP_IF_FALSE loc
      in
      let cmp, jump_else =
        cmp |> op POP loc |> compile then_ |> emit_jump Op.JUMP loc_then
      in
      cmp
      |> patch_jump jump_if
      |> op POP loc_then
      |> maybe_do compile else_
      |> patch_jump jump_else
  | SBlock stmts ->
      nest cmp loc (compile_list stmts)
  | SWhile (cond, body) ->
      let loop_start = Vect.length cmp.code in
      let cmp, jump_exit =
        cmp |> compile cond |> emit_jump Op.JUMP_IF_FALSE loc
      in
      cmp
      |> op POP loc
      |> compile body
      |> emit_loop loop_start Op.JUMP loc
      |> patch_jump jump_exit
      |> op POP loc
  | SFun (name, params, body) ->
      cmp
      |> declare_var name loc
      |> compile_function `Function name params body loc
      |> define_var name loc
  | SReturn expr -> (
    match (cmp.fn_type, expr) with
    | `Script, _ ->
        fail loc "cannot return from toplevel" cmp
    | `Init, Some _ ->
        fail loc "cannot return values from initializer" cmp
    | _, Some expr ->
        cmp |> compile expr |> op RET loc
    | _, None ->
        cmp |> op NIL loc |> op RET loc)
  | SClass (name, mets, supcls) ->
      let fetch_cls = compile (LId name, loc) in
      cmp
      |> declare_var name loc
      |> constant (String name) loc
      |> op CLASS loc
      |> define_var name loc
      |> maybe_do
           (fun sup cmp ->
             fetch_cls cmp
             |> compile sup
             |> op INHERIT loc
             |> push_ctx
             |> declare_var "super" loc)
           supcls
      |> fetch_cls
      |> compile_methods
           (if Option.is_some supcls then `Super else `Class)
           mets loc
      |> maybe_do (const @@ pop_ctx loc) supcls
      |> op POP loc
  | LNum v ->
      constant (Num v) loc cmp
  | LTrue ->
      op TRUE loc cmp
  | LFalse ->
      op FALSE loc cmp
  | LNil ->
      op NIL loc cmp
  | LString s ->
      constant (String s) loc cmp
  | LId name -> (
      if cmp.decl_var = name
      then fail loc "cannot use local variable in its own initializer" cmp
      else
        let found, cmp = resolve_variable name cmp in
        match found with
        | `FoundLocal idx ->
            cmp |> op Op.GET_LOCAL loc |> byte @@ char_of_int idx
        | `FoundUpvalue idx ->
            cmp |> op Op.GET_UPVALUE loc |> byte @@ char_of_int idx
        | `NoLocals ->
            cmp |> constant (String name) loc |> op Op.GET_GLOBAL loc)
  | BinOp (OpAnd, a, b) ->
      let cmp, jump = cmp |> compile a |> emit_jump Op.JUMP_IF_FALSE loc in
      cmp |> op Op.POP loc |> compile b |> patch_jump jump
  | BinOp (OpOr, a, b) ->
      let cmp, else_jump = cmp |> compile a |> emit_jump Op.JUMP_IF_FALSE loc in
      let cmp, end_jump = cmp |> emit_jump Op.JUMP loc in
      cmp
      |> patch_jump else_jump
      |> op Op.POP loc
      |> compile b
      |> patch_jump end_jump
  | BinOp (binop, a, b) ->
      compile_binop binop a b loc cmp
  | UniOp (uniop, a) ->
      compile_uniop uniop a loc cmp
  | Assign ((LId name, _), expr) -> (
      let found, cmp' = cmp |> compile expr |> resolve_variable name in
      match found with
      | `FoundLocal idx ->
          cmp' |> op Op.SET_LOCAL loc |> byte @@ char_of_int idx
      | `FoundUpvalue idx ->
          cmp' |> op Op.SET_UPVALUE loc |> byte @@ char_of_int idx
      | `NoLocals ->
          cmp' |> constant (String name) loc |> op Op.SET_GLOBAL loc)
  | App (callee, args) ->
      cmp
      |> compile callee
      |> flip (List.fold_left (flip compile)) args
      |> op CALL loc
      |> byte (char_of_int @@ List.length args)
  | Get (getter, name) ->
      cmp |> compile getter |> constant (String name) loc |> op GET_PROPERTY loc
  | Set (setter, name, value) ->
      cmp
      |> compile setter
      |> compile value
      |> constant (String name) loc
      |> op SET_PROPERTY loc
  | LThis _ ->
      if cmp.cls_type = `Script
      then fail loc "cannot use this outside of clsas" cmp
      else compile (LId "this", loc) cmp
  | LSuper (_, met) ->
      if cmp.cls_type <> `Super
      then fail loc "cannot use this outside of class" cmp
      else
        cmp
        |> compile (LId "this", loc)
        |> compile (LId "super", loc)
        |> op GET_SUPER loc
        |> constant (String met) loc
        |> op GET_PROPERTY loc
  | Lookup _ | Assign _ ->
      failwith
      @@ Front.str_loc loc "found "
      ^ Front.Ast.show_node_t ast
      ^ " while resolving locals"

and compile_methods typ mets loc cmp =
  let prev_cls = cmp.cls_type in
  let cmp =
    List.fold_left
      (fun cmp (name, params, body) ->
        cmp
        |> compile_function
             (if name = "init" then `Init else `Method)
             name params body loc
        |> constant (String name) loc
        |> op METHOD loc)
      { cmp with cls_type = typ }
      mets
  in
  { cmp with cls_type = prev_cls }

and compile_function typ name params body loc cmp_outer =
  let cmp_fn =
    let cmp_inner =
      {
        (make ?fn_type:(Some typ) ?fn_name:(Some name)
           ?cls_type:(Some cmp_outer.cls_type)
           ?fn_arity:(Some (List.length params))
           ())
        with
        locals = [ (match typ with `Method | `Init -> "this" | _ -> "") ];
        enclosing = Some cmp_outer;
      }
    in
    nest cmp_inner loc
      (flip
         (List.fold_left @@ fun cmp param -> cmp |> declare_var param loc)
         params
      %> flip (List.fold_left (flip compile)) body)
    |> op NIL loc
    |> op Op.RET loc
  in
  let fn = to_fun cmp_fn in
  cmp_outer
  |> op CLOSURE loc
  |> constant (FunObj fn) loc
  |> byte (char_of_int @@ List.length cmp_fn.upvalues)
  |> List.fold_right
       (fun (_, idx, is_local) cmp ->
         cmp
         |> byte (char_of_int @@ Bool.to_int is_local)
         |> byte (char_of_int @@ idx))
       cmp_fn.upvalues

and compile_binop binop a b loc cmp =
  let open Front.Ast in
  let open Op in
  cmp
  |> compile a
  |> compile b
  |>
  match binop with
  | OpAdd ->
      op ADD loc
  | OpSub ->
      op SUB loc
  | OpMul ->
      op MUL loc
  | OpDiv ->
      op DIV loc
  | OpEQ ->
      op EQUAL loc
  | OpGT ->
      op GT loc
  | OpLT ->
      op LT loc
  | OpLE ->
      op GT loc %> op NOT loc
  | OpGE ->
      op LT loc %> op NOT loc
  | OpNEQ ->
      op EQUAL loc %> op NOT loc
  | OpAnd | OpOr ->
      failwith "unreachable"

and compile_uniop uniop a loc cmp =
  let open Front.Ast in
  let open Op in
  let uniop = match uniop with OpMinus -> NEG | OpNeg -> NOT in
  cmp |> compile a |> op uniop loc
