(lang dune 3.4)
(using menhir 2.1)

(generate_opam_files true)

(name ocamlox)

(source
 (github poki-musi/ocamlox))


(package
 (name ocamlox)
 (synopsis "Lox interpreter built in OCaml")
 (description "No")
 (allow_empty)
 (depends
   ocaml
   dune
   batteries
   ocamllex
   menhir))
