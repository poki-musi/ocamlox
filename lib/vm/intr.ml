include Batteries
open Common

type value =
  | Nil
  | Num of float
  | Bool of bool
  | String of string
  | FunObj of chunk fun_body_t
  | FunNat of (t -> value array -> (value, err_t) result) fun_body_t
  | FunClos of chunk fun_body_t * value ref array
  | FunBound of value * value
  | Class of class_t
  | Instance of instance_t

and chunk = {
  code : bytes;
  constants : value array;
  lines : (int * int) array;
}

and 'callback fun_body_t = {
  sign : string;
  arity : int;
  body : 'callback;
}

and class_t = {
  name : string;
  methods : (string, value) Hashtbl.t;
  super : class_t option;
}

and instance_t = {
  cls : class_t;
  fields : (string, value) Hashtbl.t;
}

and t = {
  mutable stack : value ref Vector.t;
  mutable upvalues : value ref array;
  mutable globals : (string, value) Hashtbl.t;
  mutable ip : int;
  mutable chunk : chunk;
  mutable offset : int;
  mutable cur_fn : value option;
  mutable calls : call_t list;
}

and err_t = string * (int * int)

and call_t = {
  call_ip : int;
  call_slot : int;
  fun_body : value;
}

module Op = struct
  type t =
    | RET
    | CONST
    | LONG_CONST
    | NEG
    | NOT
    | ADD
    | SUB
    | MUL
    | DIV
    | EQUAL
    | GT
    | LT
    | POP
    | TRUE
    | FALSE
    | NIL
    | DEF_GLOBAL
    | SET_GLOBAL
    | GET_GLOBAL
    | SET_LOCAL
    | GET_LOCAL
    | GET_UPVALUE
    | SET_UPVALUE
    | GET_PROPERTY
    | SET_PROPERTY
    | GET_SUPER
    | JUMP_IF_FALSE
    | JUMP
    | CALL
    | CLOSURE
    | CLASS
    | METHOD
    | INHERIT
    | Fail
  [@@deriving enum, show { with_path = false }]

  let to_byte = to_enum %> char_of_int
  let of_byte = int_of_char %> of_enum %> Option.default Fail
end

module V = struct
  type t = value

  let rec pp ppf v = Fmt.pf ppf "%s" (show v)

  and show = function
    | Num v ->
        string_of_float v
    | Bool true ->
        "true"
    | Bool false ->
        "false"
    | String s ->
        s
    | Nil ->
        "nil"
    | Class { name; _ } ->
        "<class#" ^ name ^ ">"
    | Instance { cls = { name; _ }; fields } ->
        Fmt.(
          str "<instance#%s>{%a}" name
            (hashtbl
               ?sep:(Some (any ", "))
               (pair ?sep:(Some (any ": ")) string pp))
            fields)
    | FunObj { sign; _ } ->
        "<fun#" ^ sign ^ ">"
    | FunNat { sign; _ } ->
        "<native#" ^ sign ^ ">"
    | FunClos ({ sign; _ }, _) ->
        "<closure#" ^ sign ^ ">"
    | FunBound (fn, _) ->
        show fn

  let is_truthy = function Nil | Bool false -> false | _ -> true
end

module Chunk = struct
  type t = chunk

  let empty () = { code = Bytes.empty; constants = [||]; lines = [||] }
  let get_byte i { code; _ } = Bytes.get code i |> Op.of_byte
  let get_pos i { lines; _ } = Array.get lines i
  let num_of_bytes { code; _ } = Bytes.length code
  let num_of_constants { constants; _ } = Array.length constants

  let rec pp ppf ({ code; _ } as chk) =
    let i = ref 0 in
    while !i < Bytes.length code do
      Fmt.(pf ppf "\n");
      let offset = pp_line_next ppf (!i, chk) in
      i := offset + !i
    done

  and pp_line ppf line = ignore (pp_line_next ppf line)

  and pp_line_next ppf (i, ({ lines; code; constants } as chk)) =
    let p, c = lines.(i) in
    Fmt.(pf ppf "%-8s | " Fmt.(str "%i:%i" p c));
    Fmt.(pf ppf "%-4i | " i);
    let op = get_byte i chk in
    match op with
    | Fail ->
        Fmt.(pf ppf "Unknown opcode %x" @@ Op.to_enum op);
        0
    | POP
    | RET
    | ADD
    | SUB
    | MUL
    | DIV
    | NEG
    | NOT
    | EQUAL
    | GT
    | LT
    | TRUE
    | FALSE
    | NIL
    | CLASS
    | METHOD
    | INHERIT
    | GET_PROPERTY
    | SET_PROPERTY
    | GET_SUPER
    | DEF_GLOBAL
    | SET_GLOBAL
    | GET_GLOBAL ->
        Fmt.(pf ppf "%-18s" (Op.show op));
        1
    | (JUMP | JUMP_IF_FALSE) as typ ->
        let addr =
          if typ = JUMP
          then Bytes.get_uint8 code (i + 1)
          else
            Bytes.get_uint8 code (i + 1) + (256 * Bytes.get_uint8 code (i + 2))
        in
        Fmt.pf ppf "%-18s %04i" (Op.show typ) addr;
        3
    | (CALL | GET_UPVALUE | SET_UPVALUE | GET_LOCAL | SET_LOCAL) as typ ->
        let n = Bytes.get_uint8 code (i + 1) in
        Fmt.pf ppf "%-18s %04i" (Op.show typ) n;
        2
    | (CONST | LONG_CONST) as typ ->
        let addr =
          if typ = CONST
          then Bytes.get_uint8 code (i + 1)
          else
            Bytes.get_uint8 code (i + 1) + (256 * Bytes.get_uint8 code (i + 2))
        in
        Array.get constants addr
        |> V.show
        |> Fmt.pf ppf "%-18s %-10s" (Op.show typ);
        if typ = CONST then 2 else 3
    | CLOSURE ->
        Fmt.pf ppf "%-18s" "CLOSURE";
        if Bytes.get_uint8 code (i + 1) |> Op.of_enum |> Option.get = CONST
        then 4 + Bytes.get_uint8 code (i + 3)
        else 5 + Bytes.get_uint8 code (i + 4)

  let show = Fmt.str "%a" pp
end

let make () =
  {
    chunk = Chunk.empty ();
    ip = 0;
    stack = Vector.create ();
    upvalues = [||];
    globals = Hashtbl.create 8;
    offset = 0;
    calls = [];
    cur_fn = None;
  }

(** Function manipulation *)

let[@warning "-8"] push_fn (FunClos ({ arity; body; _ }, upvalues) as fn) vm =
  (match vm.cur_fn with
  | Some v ->
      vm.calls <-
        { fun_body = v; call_slot = vm.offset; call_ip = vm.ip } :: vm.calls
  | None ->
      ());
  vm.ip <- 0;
  vm.offset <- Vector.length vm.stack - arity - 1;
  vm.cur_fn <- Some fn;
  vm.chunk <- body;
  vm.upvalues <- upvalues;
  vm

let push_start_fn fn vm =
  Vector.add vm.stack (ref fn);
  vm.calls <- [];
  vm.cur_fn <- None;
  push_fn fn vm

let pop_fn vm =
  match vm.calls with
  | { fun_body = FunClos ({ body; _ }, _) as fun_body; call_ip; call_slot }
    :: xs ->
      vm.calls <- xs;
      vm.ip <- call_ip;
      Vector.delete_range vm.stack vm.offset (Vector.length vm.stack - vm.offset);
      vm.offset <- call_slot;
      vm.cur_fn <- Some fun_body;
      vm.chunk <- body
  | _ ->
      ()

(** Utils *)

let add_natives lst vm =
  List.iter
    (fun (sign, arity, body) ->
      Hashtbl.replace vm.globals sign @@ FunNat { sign; arity; body })
    lst;
  vm

let ( let* ) = Result.Monad.( >>= )
let return = Result.Monad.return
let fail msg vm = Error (msg, Chunk.get_pos (vm.ip - 1) vm.chunk)
let get_pos_call chunk ip = Chunk.get_pos (ip - 1) chunk
let option_fail msg vm = function Some v -> Ok v | None -> fail msg vm

(** Bytecode readers *)

let read_byte vm =
  if vm.ip >= Chunk.num_of_bytes vm.chunk
  then failwith "Reached end of chunk"
  else
    let res = Bytes.get_uint8 vm.chunk.code vm.ip in
    vm.ip <- vm.ip + 1;
    res

let read_16bit vm =
  let a = read_byte vm in
  let b = read_byte vm in
  a + (b * 256)

(** Stack manipulation *)

let safe_get_value addr stack =
  try Vector.get stack addr
  with DynArray.Invalid_arg _ ->
    failwith
      ("tried to read value at " ^ string_of_int addr ^ " but there was nothing")

let push_value v vm = Vector.add vm.stack @@ ref v

let peek_value vm =
  if Vector.empty vm.stack
  then fail "stack emptied out" vm
  else Ok !(Vector.last vm.stack)

let peek_value_at addr vm =
  if Vector.empty vm.stack
  then fail "stack emptied out" vm
  else Ok !(safe_get_value (vm.offset + addr) vm.stack)

let modify_value_at addr v vm =
  if Vector.length vm.stack <= vm.offset + addr
  then fail "accessing slot outside of stack" vm
  else (
    safe_get_value (vm.offset + addr) vm.stack := v;
    Ok ())

let pop_value vm =
  if Vector.empty vm.stack
  then fail "stack emptied out" vm
  else
    let e = !(Vector.last vm.stack) in
    Vector.delete_last vm.stack;
    Ok e

let as_string ?(err = "must be string") vm = function
  | String v ->
      Ok v
  | _ ->
      fail err vm

let as_obj ?(err = "can only accept instances") vm = function
  | Instance v ->
      Ok v
  | _ ->
      fail err vm

let as_cls ?(err = "can only accept classes") vm = function
  | Class v ->
      Ok v
  | _ ->
      fail err vm

let as_closure ?(err = "can only accept closures") vm = function
  | FunClos (x, y) ->
      Ok (x, y)
  | _ ->
      fail err vm

(** OOP stuff *)

let getter name obj vm =
  Hashtbl.find_option obj.fields name
  |> Option.map_default_delayed Result.ok
     @@ fun () ->
     Hashtbl.find_option obj.cls.methods name
     |> Option.map_default_delayed (fun met ->
            Ok (FunBound (met, Instance obj)))
        @@ fun () -> fail ("property " ^ name ^ " not found") vm

let setter name value obj = Hashtbl.replace obj.fields name value

(** High level functions *)

let binop f vm =
  let* b = pop_value vm in
  let* a = pop_value vm in
  match (a, b) with
  | Num a, Num b ->
      push_value (f a b) vm;
      return None
  | _ ->
      fail "operator is incompatible with given values" vm

let[@warning "-8"] print_trace (err, (l, p)) vm =
  Fmt.(pf stderr "error:%i:%i: %s@." l (p + 1) err);
  vm.calls
  |> List.iter
     @@ fun { call_ip; fun_body = FunClos ({ body; sign; _ }, _); _ } ->
     let l, p = get_pos_call body call_ip in
     Fmt.(pf stderr "in [%s:%i:%i]@." sign l (p + 1))

(** Main function *)

let rec run vm =
  (match run_loop vm with Error e -> print_trace e vm | _ -> ());
  Vector.clear vm.stack

and run_loop vm =
  (* Fmt.( *)
  (*   pr "%a@." (brackets (array ?sep:(Some (any ",")) V.pp)) *)
  (*   @@ Vector.to_array *)
  (*   @@ Vector.map ( ! ) vm.stack); *)
  (* Fmt.(pr "%i | %a@." vm.ip Chunk.pp_line (vm.ip, vm.chunk)); *)
  match next_step vm with
  | Ok None ->
      run_loop vm
  | Ok (Some v) ->
      Ok v
  | Error _ as e ->
      e

and next_step vm =
  let ( >>= ) = Result.Infix.( >>= ) in
  if vm.ip >= Chunk.num_of_bytes vm.chunk
  then fail "stepped out of the chunk when advancing" vm
  else
    match read_byte vm |> Op.of_enum |> Option.get with
    | Fail ->
        fail "unknown opcode" vm
    | RET ->
        let* v = pop_value vm in
        return (Some v)
    | POP ->
        let* _ = pop_value vm in
        return None
    | (CONST | LONG_CONST) as cnst ->
        let addr = if cnst = CONST then read_byte vm else read_16bit vm in
        push_value vm.chunk.constants.(addr) vm;
        return None
    | TRUE ->
        push_value (Bool true) vm;
        return None
    | FALSE ->
        push_value (Bool false) vm;
        return None
    | NIL ->
        push_value Nil vm;
        return None
    | NOT ->
        let* v = pop_value vm in
        push_value V.(Bool (neg is_truthy v)) vm;
        return None
    | NEG -> (
        let* v = pop_value vm in
        match v with
        | Num v ->
            push_value (Num (-.v)) vm;
            return None
        | _ ->
            fail "cannot negate non-integer" vm)
    | ADD -> (
        let* b = pop_value vm in
        let* a = pop_value vm in
        match (a, b) with
        | Num a, Num b ->
            let ret = Num (a +. b) in
            push_value ret vm;
            return None
        | String a, String b ->
            push_value (String (a ^ b)) vm;
            return None
        | _ ->
            fail "operator is incompatible with given values" vm)
    | SUB ->
        binop (fun a b -> Num (a -. b)) vm
    | MUL ->
        binop (fun a b -> Num (a *. b)) vm
    | DIV ->
        binop (fun a b -> Num (a /. b)) vm
    | LT ->
        binop (fun a b -> Bool (a < b)) vm
    | GT ->
        binop (fun a b -> Bool (a > b)) vm
    | EQUAL ->
        let* b = pop_value vm in
        let* a = pop_value vm in
        push_value (Bool (a = b)) vm;
        return None
    | JUMP_IF_FALSE ->
        let addr = read_16bit vm in
        let* v = peek_value vm in
        if neg V.is_truthy v then vm.ip <- addr;
        return None
    | JUMP ->
        let addr = read_16bit vm in
        vm.ip <- addr;
        return None
    | DEF_GLOBAL ->
        let* name = pop_value vm >>= as_string vm in
        let* value = pop_value vm in
        if Hashtbl.mem vm.globals name
        then fail "global variable already declared" vm
        else (
          Hashtbl.replace vm.globals name value;
          return None)
    | GET_GLOBAL ->
        let* name = pop_value vm >>= as_string vm in
        let* value =
          Hashtbl.find_option vm.globals name
          |> option_fail ("global variable not found '" ^ name ^ "'") vm
        in
        push_value value vm;
        return None
    | SET_GLOBAL ->
        let* name = pop_value vm >>= as_string vm in
        let* value = pop_value vm in
        if not (Hashtbl.mem vm.globals name)
        then fail ("variable '" ^ name ^ "' not found") vm
        else (
          Hashtbl.replace vm.globals name value;
          push_value value vm;
          return None)
    | GET_LOCAL ->
        let addr = read_byte vm in
        let* res = peek_value_at addr vm in
        push_value res vm;
        return None
    | SET_LOCAL ->
        let addr = read_byte vm in
        let* res = peek_value vm in
        let* _ = modify_value_at addr res vm in
        return None
    | GET_UPVALUE ->
        let slot = read_byte vm in
        (* Fmt.( *)
        (*   pr "upvalues: %a@." *)
        (*     (brackets (array V.pp)) *)
        (* (Array.map ( ! ) vm.upvalues)); *)
        push_value !(vm.upvalues.(slot)) vm;
        return None
    | SET_UPVALUE ->
        let* value = pop_value vm in
        let slot = read_byte vm in
        vm.upvalues.(slot) := value;
        return None
    | GET_PROPERTY ->
        let* name = pop_value vm >>= as_string vm in
        let* obj =
          pop_value vm
          >>= as_obj ?err:(Some "only instances can have fields") vm
        in
        let* value = getter name obj vm in
        push_value value vm;
        return None
    | SET_PROPERTY ->
        let* name = pop_value vm >>= as_string vm in
        let* value = pop_value vm in
        let* obj =
          pop_value vm
          >>= as_obj ?err:(Some "only instances can have fields") vm
        in
        setter name value obj;
        push_value value vm;
        return None
    | CALL ->
        let nargs = read_byte vm in
        let* callee =
          let i = Vector.length vm.stack - nargs - 1 in
          if i < 0
          then fail "peeked out of range" vm
          else Ok !(safe_get_value i vm.stack)
        in
        let* res = call_value callee nargs vm in
        push_value res vm;
        return None
    | CLOSURE ->
        let cnst = read_byte vm |> Op.of_enum |> Option.get in
        let addr = if cnst = CONST then read_byte vm else read_16bit vm in

        let[@warning "-8"] (FunObj body) = vm.chunk.constants.(addr) in
        let len = read_byte vm in
        let new_ups = Array.create len (ref Nil) in
        push_value (FunClos (body, new_ups)) vm;

        for i = 0 to len - 1 do
          let is_local = read_byte vm in
          let idx = read_byte vm in
          new_ups.(i) <-
            (match is_local with
            | 0 ->
                vm.upvalues.(idx)
            | 1 ->
                safe_get_value (vm.offset + idx) vm.stack
            | _ ->
                failwith "no")
        done;

        return None
    | METHOD ->
        let* name = pop_value vm >>= as_string vm in
        let* met, upv = pop_value vm >>= as_closure vm in
        let* cls = peek_value vm >>= as_cls vm in
        Hashtbl.replace cls.methods name @@ FunClos (met, upv);
        return None
    | CLASS ->
        let* name = pop_value vm >>= as_string vm in
        push_value (Class { name; methods = Hashtbl.create 0; super = None }) vm;
        return None
    | INHERIT ->
        let* sup = pop_value vm >>= as_cls vm in
        let* cls = pop_value vm >>= as_cls vm in
        if sup = cls
        then fail "cannot inherit class from itself" vm
        else (
          Hashtbl.bindings sup.methods
          |> List.iter (fun (k, v) -> Hashtbl.add cls.methods k v);
          push_value (Class sup) vm;
          return None)
    | GET_SUPER ->
        let* sup = pop_value vm >>= as_cls vm in
        let* obj = pop_value vm >>= as_obj vm in
        push_value (Instance { obj with cls = sup }) vm;
        return None

and call_value callee nargs vm =
  match callee with
  | FunClos _ as fn ->
      call_fun_obj fn nargs vm
  | FunNat { arity; body; _ } ->
      let* _ = guard_arity vm arity nargs in
      let ini = Vector.length vm.stack - nargs in
      let* res =
        body vm Vector.(sub vm.stack ini nargs |> map ( ! ) |> to_array)
      in
      Vector.delete_range vm.stack (ini - 1) (nargs + 1);
      return res
  | FunBound (met, this) ->
      Vector.get vm.stack (Vector.length vm.stack - nargs - 1) := this;
      call_value met nargs vm
  | Class cls -> (
      let ret = Instance { cls; fields = Hashtbl.create 0 } in
      match Hashtbl.find_option cls.methods "init" with
      | Some init ->
          let* _ = call_value (FunBound (init, ret)) nargs vm in
          return ret
      | None ->
          let* _ = guard_arity vm nargs 0 in
          let ini = Vector.length vm.stack - nargs in
          Vector.delete_range vm.stack (ini - 1) (nargs + 1);
          return ret)
  | _ ->
      fail "callee is not callable" vm

and[@warning "-8"] call_fun_obj (FunClos ({ arity; _ }, _) as fn) nargs vm =
  let* _ = guard_arity vm arity nargs in
  ignore @@ push_fn fn vm;
  let* res = run_loop vm in
  pop_fn vm;
  return res

and guard_arity vm arity n_args =
  if arity = n_args
  then Ok ()
  else
    fail
      ("number of arguments does not match (required "
      ^ string_of_int arity
      ^ ", has "
      ^ string_of_int n_args
      ^ ")")
      vm
