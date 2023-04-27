(lang dune 3.1)
(name minimal)
(using menhir 2.0)

(generate_opam_files true)

(package
  (name minimal)
  (maintainers Garrigue HyunggyuJang)
  (description "study project")
  (depends
    ocaml
    fmt
    ppx_inline_test
    ppx_expect
    (alcotest :with-test)
    (ocamlformat :with-test)))
