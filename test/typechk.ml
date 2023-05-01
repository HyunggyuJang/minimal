module Typechk = Minimal.Typechk
open Minimal.Types
open Minimal.Common
open Utils
open Minimal.Define

let type_expr_testable = Alcotest.testable pp_type_expr ( = )
let type_info_testable = Alcotest.testable pp_type_info ( = )
let value_info_testable = Alcotest.testable pp_value_info ( = )

let test_constants () =
  let test_cases =
    [ "int", Tconstr ({ name = "int"; index = 1 }, []), "5"
    ; "char", Tconstr ({ name = "char"; index = 2 }, []), "'c'"
    ; "float", Tconstr ({ name = "float"; index = 3 }, []), "5.8"
    ]
  in
  List.iter
    (fun (name, expected, input) ->
      parse_exp input
      |> Typechk.type_expression StrMap.empty
      |> Alcotest.check type_expr_testable name expected)
    test_cases
;;

let test_record () =
  do_phrase {| type Test = { mutable a: int } |};
  Alcotest.check
    type_info_testable
    "After declaring record type"
    { ti_params = []
    ; ti_res = Tconstr ({ name = "Test"; index = 9 }, [])
    ; ti_kind =
        Krecord [ "a", Tconstr ({ name = "int"; index = 1 }, []), Mutable ]
    }
  @@ Hashtbl.find types "Test";
  do_phrase {| val test = { a = 8 } |};
  Alcotest.check
    value_info_testable
    "After bind instance of record type"
    { vi_type =
        Tvar
          { link = Some (Tconstr ({ name = "Test"; index = 9 }, []))
          ; level = 1
          }
    ; vi_access = Immutable
    }
  @@ StrMap.find "test" !values;
  let test_cases =
    [ "Query instance", Tconstr ({ name = "Test"; index = 9 }, []), "test"
    ; "Access field", Tconstr ({ name = "int"; index = 1 }, []), "test.a"
    ]
  in
  List.iter
    (fun (name, expected, input) ->
      parse_exp input
      |> Typechk.type_expression !values
      |> Alcotest.check type_expr_testable name expected)
    test_cases
;;

let test_mutual_typedef () =
  do_phrase {| type t1 = Test1 of t2 and t2 = Test2 of t1 |};
  List.iter
    (fun (name, expected, input) ->
      Alcotest.check type_info_testable name expected
      @@ Hashtbl.find types input)
    [ ( "t1 type"
      , { ti_params = []
        ; ti_res = Tconstr ({ name = "t1"; index = 11 }, [])
        ; ti_kind =
            Kvariant [ "Test1", [ Tconstr ({ name = "t2"; index = 12 }, []) ] ]
        }
      , "t1" )
    ; ( "t2 type"
      , { ti_params = []
        ; ti_res = Tconstr ({ name = "t2"; index = 12 }, [])
        ; ti_kind =
            Kvariant [ "Test2", [ Tconstr ({ name = "t1"; index = 11 }, []) ] ]
        }
      , "t2" )
    ]
;;

let () =
  Alcotest.run
    "Typechk"
    [ "expression", [ Alcotest.test_case "const" `Quick test_constants ]
    ; ( "type declaration"
      , [ Alcotest.test_case "record" `Quick test_record
        ; Alcotest.test_case "mutual" `Quick test_mutual_typedef
        ] )
    ]
;;
