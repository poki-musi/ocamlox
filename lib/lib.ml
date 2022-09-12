open Batteries
module Front = Front
module Tw = Tw
module Vm = Vm

let with_file_in f name =
  let fh = open_in name in
  let res = f fh in
  close_in fh;
  res

let rec prompt f =
  Fmt.(pr "lox> @?");
  let line = IO.read_line stdin in
  if neg String.is_empty line
  then (
    f (IO.input_string line);
    prompt f)
  else ()

let parse_cli =
  let output_type = ref `VM in
  let set_out str = Arg.Unit (fun () -> output_type := str) in

  let pos_args = ref [] in

  let usage_msg = "lox [file]" in
  let speclist =
    [
      ("-ast", set_out `Ast, "Print AST.");
      ("-tw", set_out `Treewalk, "Compute code with a treewalker.");
      ("-byte", set_out `VM, "Compute code with a bytecode VM.");
    ]
  in
  fun () ->
    Arg.parse speclist (fun x -> pos_args := x :: !pos_args) usage_msg;
    (List.rev !pos_args, !output_type)

let main () =
  let args, type_ = parse_cli () in
  let proc =
    Option.may
    @@
    match type_ with
    | `Ast ->
        List.map Front.Ast.show_node %> String.join "\n" %> Fmt.pr "%s"
    | `Treewalk ->
        Tw.main ()
    | `VM ->
        Vm.main ()
  in
  if neg List.is_empty args then
    args |> List.iter (with_file_in Front.parse %> proc)
  else
    prompt @@ Front.parse %> proc
