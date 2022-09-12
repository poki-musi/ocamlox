open Batteries

let main () =
  let open Intr in
  let rec aux lst =
    lst
    |> Resolution.main
    |> Option.may (fun lst ->
           try Eval.eval_list vm lst with Front.RuntimeErr -> ())
  and vm =
    let add_natives lst vm =
      lst
      |> List.iter (fun (name, arity, callback) ->
             set_global vm name @@ FNative { name; arity; callback });
      vm
    in
    make ()
    |> add_natives
         [
           ("clock", 0, fun _ _ -> Num (Unix.time ()));
           ( "show",
             1,
             fun _ args ->
               (match args with
               | [ arg ] ->
                   Fmt.pr "%s" (stringify_value arg)
               | _ ->
                   failwith "unreachable");
               Nil );
           ( "print",
             1,
             fun _ args ->
               (match args with
               | [ arg ] ->
                   Fmt.pr "%s\n" (stringify_value arg)
               | _ ->
                   failwith "unreachable");
               Nil );
           ( "to_str",
             1,
             fun _ args ->
               match args with
               | [ arg ] ->
                   Str (stringify_value arg)
               | _ ->
                   failwith "unreachable" );
           ( "meow",
             0,
             fun _ _ ->
               for _ = 0 to 1000 do
                 Fmt.(pr "Meow\n")
               done;
               Nil );
           ( "show_tree",
             1,
             fun _ args ->
               match args with
               | [ Intr.FFunc { name; callback = { body; _ }; _ } ] ->
                   Fmt.pr "-- %s --@." name;
                   Fmt.(pr "%a@." (list Front.Ast.pp_node) body);
                   Intr.Nil
               | _ ->
                   Fmt.pr "warning: value given is not an error";
                   Intr.Nil );
         ]
  in
  aux
