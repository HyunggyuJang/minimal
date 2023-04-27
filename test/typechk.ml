module Typechk = Minimal.Typechk
open Minimal.Types
open Minimal.Common
open Utils

let type_expr_testable = Alcotest.testable pp_type_expr ( = )

let test_constants () =
  let test_cases =
    [ "int", Tconstr ({ name = "int"; index = 1 }, []), "5"
    ; "char", Tconstr ({ name = "char"; index = 2 }, []), "'c'"
    ; "float", Tconstr ({ name = "float"; index = 3 }, []), "5.8"
    ]
  in
  List.iter
    (fun (name, expected, input) ->
      parse input
      |> Typechk.expression StrMap.empty
      |> Alcotest.check type_expr_testable name expected)
    test_cases
;;

let () =
  Alcotest.run
    "Typechk"
    [ "expression", [ Alcotest.test_case "const" `Quick test_constants ] ]
;;
