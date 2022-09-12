open Front.Ast
open Batteries

type t = {
  mutable scopes : (string, bool) Hashtbl.t list;
  mutable func_type : t_func_ctx;
  mutable in_class : t_class_ctx;
  mutable error : bool;
}

and t_func_ctx =
  | FNone
  | FMethod
  | FFunc
  | FInit

and t_class_ctx =
  | CNone
  | CClass
  | CSubclass

let error rsv (loc, _) msg =
  let l, p = Front.Ast.span_of_pos loc in
  Front.fmt Fmt.stderr l p msg;
  rsv.error <- true

let find rsv name = List.find_map_opt (flip Hashtbl.find_option name) rsv.scopes

let define rsv name =
  if neg List.is_empty rsv.scopes
  then
    let scp = List.hd rsv.scopes in
    Hashtbl.replace scp name true

let declare rsv name loc =
  if List.is_empty rsv.scopes
  then ()
  else
    let scp = List.hd rsv.scopes in
    if Hashtbl.mem scp name
    then
      error rsv loc
        ("already a variable exists with this same '"
        ^ name
        ^ "' name within this scope")
    else Hashtbl.replace scp name false

let resolve_local rsv name =
  rsv.scopes
  |> List.mapi (fun i v -> (i, v))
  |> List.find_map_opt (fun (i, scope) ->
         if Hashtbl.mem scope name then Some i else None)
  |> fun v -> Lookup (v, name)

let with_scope rsv f =
  rsv.scopes <- Hashtbl.create 8 :: rsv.scopes;
  let res = f () in
  rsv.scopes <- List.tl rsv.scopes;
  res

let rec resolve_function rsv (_, params, body) ftype loc =
  let prev_type = rsv.func_type in
  rsv.func_type <- ftype;

  let body =
    with_scope rsv (fun () ->
        params
        |> List.iter (fun param ->
               declare rsv param loc;
               define rsv param);
        resolve_list rsv body)
  in

  rsv.func_type <- prev_type;
  body

and resolve_class rsv name methods supcls loc =
  let prev_class_ctx = rsv.in_class in
  rsv.in_class <- (match supcls with Some _ -> CSubclass | None -> CClass);

  declare rsv name loc;
  let supcls' = Option.map (resolve rsv) supcls in
  define rsv name;

  let super_envelop f =
    match supcls with
    | Some _ ->
        with_scope rsv (fun () ->
            define rsv "super";
            f ())
    | None ->
        f ()
  in

  let methods' =
    super_envelop (fun () ->
        with_scope rsv (fun () ->
            define rsv "this";
            methods
            |> List.map (fun ((name, params, _) as met) ->
                   ( name,
                     params,
                     resolve_function rsv met
                       (if name = "init" then FInit else FMethod)
                       loc ))))
  in

  rsv.in_class <- prev_class_ctx;
  SClass (name, methods', supcls')

and resolve_list rsv stmts = List.map (resolve rsv) stmts

and resolve rsv (ast, loc) =
  let resolve = resolve rsv in
  flip Tuple2.make loc
  @@
  match ast with
  | LNum _ | LString _ | LTrue | LFalse | LNil ->
      ast
  | LId name ->
      if neg List.is_empty rsv.scopes && not (find rsv name |? true)
      then
        error rsv loc
          ("cannot read local variable within its own initializer ("
          ^ name
          ^ ")")
        |> const ast
      else resolve_local rsv name
  | Get (obj, name) ->
      Get (resolve obj, name)
  | Set (obj, name, expr) ->
      let expr' = resolve expr in
      let obj' = resolve obj in
      Set (obj', name, expr')
  | App (callee, args) ->
      App (resolve callee, List.map resolve args)
  | BinOp (op, a, b) ->
      BinOp (op, resolve a, resolve b)
  | UniOp (op, a) ->
      UniOp (op, resolve a)
  | Assign (name, expr) ->
      Assign (resolve name, resolve expr)
  | LThis id ->
      if rsv.in_class = CNone
      then error rsv loc "can't use 'this' outside of class" |> const ast
      else resolve id |> fst
  | LSuper (id, name) -> (
    match rsv.in_class with
    | CNone ->
        error rsv loc "can't use 'super' outside of class";
        ast
    | CClass ->
        error rsv loc "can't use 'super' in class with no superclass";
        ast
    | CSubclass ->
        LSuper (resolve id, name))
  | SBlock stmts ->
      SBlock (with_scope rsv (fun () -> resolve_list rsv stmts))
  | SVar (var, expr) ->
      SVar (var, Option.map resolve expr)
  | SFun ((name, params, _) as fn) ->
      declare rsv name loc;
      define rsv name;
      SFun (name, params, resolve_function rsv fn FFunc loc)
  | SExpr expr ->
      SExpr (resolve expr)
  | SIf (cond, then_, else_) ->
      SIf (resolve cond, resolve then_, Option.map resolve else_)
  | SReturn expr -> (
    match (rsv.func_type, expr) with
    | FNone, _ ->
        error rsv loc "can't return from top level" |> const ast
    | FInit, Some _ ->
        error rsv loc "can't return values from init method" |> const ast
    | _, _ ->
        SReturn (Option.map resolve expr))
  | SWhile (cond, block) ->
      SWhile (resolve cond, resolve block)
  | SClass (name, methods, supcls) ->
      resolve_class rsv name methods supcls loc
  | Lookup _ ->
      failwith @@ Front.str_loc loc "found Lookup while resolving locals"

let main stmts =
  let rsv =
    { scopes = []; func_type = FNone; in_class = CNone; error = false }
  in

  let stmts = resolve_list rsv stmts in
  if rsv.error then None else Some stmts
