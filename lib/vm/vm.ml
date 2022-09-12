include Batteries
module Intr = Intr
module Chunk = Intr.Chunk
module Compiler = Compiler

let main () =
  let rec aux ast =
    Compiler.main (snd @@ List.hd ast) ast
    |> Option.may (fun (fn, _) -> intr () |> Intr.push_start_fn fn |> Intr.run)
  and intr =
    let open Intr in
    let obj = ref None in
    fun () ->
      match !obj with
      | Some v ->
          v
      | None ->
          Intr.make ()
          |> Intr.add_natives
               [
                 ("clock", 0, fun _ _ -> return @@ Num (Unix.time ()));
                 ( "show",
                   1,
                   fun _ args ->
                     Fmt.pr "%s" (V.show args.(0));
                     return Nil );
                 ( "print",
                   1,
                   fun _ args ->
                     Fmt.pr "%s@." (V.show args.(0));
                     return Nil );
                 ("to_str", 1, fun _ args -> return @@ String (V.show args.(0)));
                 ( "meow",
                   0,
                   fun _ _ ->
                     for _ = 0 to 1000 do
                       Fmt.(pr "Meow\n")
                     done;
                     return Nil );
                 ( "bytecode",
                   1,
                   fun vm -> function[@warning "-8"]
                     | [|
                         ( FunObj { sign; body; _ }
                         | FunClos ({ sign; body; _ }, _) );
                       |] ->
                         Fmt.(pr ">> %s <<%a@." sign Chunk.pp body);
                         return Nil
                     | _ ->
                         fail "needs compiled bytecode object" vm );
               ]
          |> tap (fun v -> obj := Some v)
  in
  aux
