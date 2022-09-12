open Front.Ast
open Batteries
module VM = Intr

exception ReturnExn of VM.value

let rec eval_list vm = List.iter (eval vm %> ignore)

and eval_var vm name dist loc =
  let res =
    match dist with
    | Some dist ->
        VM.lookup vm name dist
    | None ->
        VM.get_global vm name
  in
  match res with
  | Some v ->
      v
  | None ->
      VM.runtime_loc loc @@ "missing variable called '" ^ name ^ "'"

and eval vm (ast, loc) =
  let open VM in
  match ast with
  | LNum num ->
      Num num
  | LString name ->
      Str name
  | LTrue ->
      Bool true
  | LFalse ->
      Bool false
  | LNil ->
      Nil
  | LThis id ->
      eval vm id
  | LSuper ((Lookup (lookup, name), _), met) ->
      let[@warning "-8"] (Class cls) = eval_var vm name lookup loc in
      let fields =
        eval_var vm "this" Option.(map pred lookup) loc
        |> as_instance
             ?err:
               (Some
                  "unreachable state in which 'this' variable is not an \
                   instance") loc
        |> snd
      in
      VM.get_method (cls, fields) met loc
  | UniOp (op, a) ->
      eval_uniop vm op a loc
  | BinOp (op, a, b) ->
      eval_binop vm op a b loc
  | Set (obj, name, expr) ->
      let expr' = eval vm expr in
      let _, fields = eval vm obj |> VM.as_instance loc in
      VM.set_obj_field fields name expr';
      expr'
  | Get (obj, name) ->
      let obj = eval vm obj |> as_instance loc in
      VM.get_obj_field obj name loc
  | Lookup (lookup, name) ->
      eval_var vm name lookup loc
  | App (callee, args) ->
      eval_app vm loc callee args
  | Assign ((Lookup (lookup, name), _), expr) ->
      eval_assign vm name lookup expr
  | SExpr expr ->
      eval vm expr
  | SVar (name, expr) ->
      expr |> Option.map_default (eval vm) VM.Nil |> VM.declare vm name;
      VM.Nil
  | SIf (cond, then_, else_) ->
      if cond |> eval vm |> VM.is_truthy
      then eval vm then_
      else if Option.is_some else_
      then eval vm (Option.get else_)
      else VM.Nil
  | SWhile (cond, body) ->
      while cond |> eval vm |> VM.is_truthy do
        ignore (eval vm body)
      done;
      VM.Nil
  | SFun (name, params, body) ->
      VM.add_fobject name { params; body; closure = vm.scope };
      VM.Nil
  | SReturn expr ->
      let ret = Option.map_default (eval vm) VM.Nil expr in
      raise (ReturnExn ret)
  | SClass (name, methods, supercls) ->
      VM.add_class vm name methods
        (supercls
        |> Option.map (fun cls_id -> cls_id |> eval vm |> as_class loc))
  | SBlock lst ->
      eval_list vm lst;
      VM.Nil
  | LId _ | LSuper _ | Assign _ ->
      unreachable_node loc ast

and eval_uniop vm op expr loc =
  let expr = eval vm expr in
  match (op, expr) with
  | OpMinus, VM.Num expr ->
      VM.Num (-.expr)
  | OpMinus, _ ->
      VM.runtime_loc loc "expression given is not a number"
  | OpNeg, expr ->
      VM.Bool (neg VM.is_truthy expr)

and eval_binop vm op a b loc =
  match op with
  | OpAnd ->
      let a = eval vm a in
      if VM.is_truthy a then eval vm b else a
  | OpOr ->
      let a = eval vm a in
      if VM.is_truthy a then a else eval vm b
  | _ -> (
      let a' = eval vm a in
      let b' = eval vm b in
      match (op, a', b') with
      | OpAdd, VM.Num a, VM.Num b ->
          VM.Num (a +. b)
      | OpSub, VM.Num a, VM.Num b ->
          VM.Num (a -. b)
      | OpMul, VM.Num a, VM.Num b ->
          VM.Num (a *. b)
      | OpDiv, VM.Num a, VM.Num b ->
          VM.Num (a /. b)
      | OpGT, VM.Num a, VM.Num b ->
          VM.Bool (a > b)
      | OpGE, VM.Num a, VM.Num b ->
          VM.Bool (a >= b)
      | OpLT, VM.Num a, VM.Num b ->
          VM.Bool (a < b)
      | OpLE, VM.Num a, VM.Num b ->
          VM.Bool (a <= b)
      | OpAdd, VM.Str a, VM.Str b ->
          VM.Str (a ^ b)
      | ( ((OpAdd | OpSub | OpMul | OpDiv | OpGT | OpGE | OpLT | OpLE) as tkn),
          a,
          b ) ->
          VM.runtime_loc loc
          @@ "operator '"
          ^ Front.Ast.show_binop tkn
          ^ "' is not compatible with values '"
          ^ VM.stringify_value a
          ^ "' and '"
          ^ VM.stringify_value b
          ^ "'"
      | OpEQ, a, b ->
          VM.Bool (a = b)
      | OpNEQ, a, b ->
          VM.Bool (a <> b)
      | (OpAnd | OpOr), _, _ ->
          VM.unreachable_node loc (BinOp (op, a, b)))

and eval_assign vm name dist expr =
  let res = eval vm expr in
  (match dist with
  | Some dist ->
      VM.assign vm name dist res
  | None ->
      VM.set_global vm name res);
  res

and args_guard n_args r_args loc =
  if n_args <> r_args
  then
    VM.runtime_loc loc
    @@ "expected "
    ^ string_of_int n_args
    ^ " arguments but got %i"
    ^ string_of_int r_args;
  ()

and eval_app vm loc call args =
  let open VM in
  let call = eval vm call in
  let r_args = List.length args in
  match call with
  | FNative { arity; callback; _ } ->
      args_guard arity r_args loc;
      callback vm (List.map (eval vm) args)
  | FFunc { callback; is_init; _ } ->
      args_guard (List.length callback.params) r_args loc;
      call_function vm args is_init callback
  | Class ({ init; _ } as cls) ->
      let obj = Instance (cls, Hashtbl.create 8) in
      (match init with
      | Some ({ closure; _ } as fn) ->
          let top, closure = VM.new_env closure in
          Hashtbl.replace top "this" obj;
          ignore @@ call_function vm args false { fn with closure }
      | None ->
          ());
      obj
  | o ->
      VM.runtime_loc loc
        Fmt.(str "tried to call with a %s" (stringify_value o))

and call_function vm args is_init { params; body; closure } =
  let open VM in
  let args = List.map (eval vm) args in
  with_scope closure vm (fun () ->
      (* Enter new local scope *)
      nest vm;

      (* Assign arguments to local variables *)
      Seq.(
        zip (of_list params) (of_list args)
        |> iter (fun (name, arg) -> VM.declare vm name arg));

      (* Compute function *)
      let ret = try eval_list vm body |> const Nil with ReturnExn e -> e in

      (* Exit local scope *)
      pop vm;
      if is_init then Hashtbl.find (List.hd vm.scope) "this" else ret)
