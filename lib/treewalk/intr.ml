open Batteries

type vm = {
  mutable scope : env list;
  mutable global : env;
}

and value =
  | Nil
  | Num of float
  | Str of string
  | Bool of bool
  | FNative of {
      name : string;
      arity : int;
      callback : vm -> value list -> value;
    }
  | FFunc of {
      name : string;
      is_init : bool;
      callback : func_t;
    }
  | Class of class_t
  | Instance of class_t * (string, value) Hashtbl.t

and env = (string, value) Hashtbl.t

and class_t = {
  super : class_t option;
  name : string;
  init : func_t option;
  methods : (string, func_t) Hashtbl.t;
}

and func_t = {
  params : string list;
  body : Front.Ast.node list;
  closure : env list;
}

let runtime_loc (loc, _p) msg =
  let l, p = Front.Ast.span_of_pos loc in
  Front.runtime l p msg

let unreachable_node loc ast =
  runtime_loc loc
  @@ "found invalid node during runtime: "
  ^ Front.Ast.show_node_t ast

let as_instance ?(err = "only instances have properties") loc = function
  | Instance (cls, fields) ->
      (cls, fields)
  | _ ->
      runtime_loc loc err

let as_class ?(err = "value must be a class") loc = function
  | Class cls ->
      cls
  | _ ->
      runtime_loc loc err

let is_truthy = function Nil | Bool false -> false | _ -> true

let rec stringify_value = function
  | Nil ->
      "nil"
  | Bool true ->
      "true"
  | Bool false ->
      "false"
  | Str str ->
      str
  | Num num ->
      string_of_float num
  | FNative { name; _ } ->
      "<native#" ^ name ^ ">"
  | FFunc { name; _ } ->
      "<fun#" ^ name ^ ">"
  | Class { name; _ } ->
      "<class#" ^ name ^ ">"
  | Instance ({ name; _ }, fields) ->
      Fmt.(
        str "<instance#%s>{%a}" name
          (hashtbl
             ?sep:(Some (any ", "))
             (pair ?sep:(Some (any ": ")) string pp_value))
          fields)

and pp_value f v = Fmt.pf f "%s" @@ stringify_value v

let new_env scp =
  let new_scope = Hashtbl.create 8 in
  (new_scope, new_scope :: scp)

let make () =
  let global = Hashtbl.create 8 in
  { global; scope = [ global ] }

let nest vm = vm.scope <- snd @@ new_env vm.scope

let pop vm =
  vm.scope <-
    (match vm.scope with
    | _ :: (_ :: _ as enveloping) ->
        enveloping
    | [ _ ] ->
        vm.scope
    | _ ->
        failwith "unreachable (popping global scope)")

let set_global vm = Hashtbl.replace vm.global
let get_global vm = Hashtbl.find_option vm.global
let declare vm k v = Hashtbl.replace (List.hd vm.scope) k v

let lookup vm id dist =
  List.drop dist vm.scope |> List.hd |> flip Hashtbl.find_option id

let assign vm id dist res =
  List.drop dist vm.scope |> List.hd |> flip (flip Hashtbl.replace id) res

let with_scope scope vm fn =
  let prev_scope = vm.scope in
  vm.scope <- scope;
  let res = fn () in
  vm.scope <- prev_scope;
  res

let rec add_fobject name ({ closure; _ } as func_v) =
  let[@warning "-8"] (head :: _) = closure in
  Hashtbl.replace head name @@ make_fobject name func_v

and make_fobject ?(is_init = false) name callback =
  FFunc { name; is_init; callback }

let add_class vm name methods super =
  let methods =
    let closure =
      match[@warning "-8"] super with
      | Some super ->
          let head, scope' = new_env vm.scope in
          Hashtbl.replace head "super" @@ Class super;
          scope'
      | None ->
          vm.scope
    in
    methods
    |> List.map (fun (name, params, body) -> (name, { params; body; closure }))
    |> Hashtbl.of_list
  in
  let init = Hashtbl.find_option methods "init" in
  let cls = Class { super; name; init; methods } in
  declare vm name cls;
  cls

let make_method obj name ({ closure; _ } as func_v) =
  let head, closure = new_env closure in
  Hashtbl.replace head "this" obj;
  make_fobject ?is_init:(Some (name = "init")) name { func_v with closure }

let rec get_obj_field ((_, fields) as obj) name loc =
  match Hashtbl.find_option fields name with
  | Some value ->
      value
  | None ->
      get_method obj name loc

and set_obj_field = Hashtbl.replace

and get_method (cls, fields) name loc =
  match (Hashtbl.find_option cls.methods name, cls.super) with
  | Some fn, _ ->
      make_method (Instance (cls, fields)) name fn
  | None, Some cls' ->
      get_method (cls', fields) name loc
  | _ ->
      runtime_loc loc ("undefined property '" ^ name ^ "'")
