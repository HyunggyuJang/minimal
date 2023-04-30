module Typechk = Minimal.Typechk
open Minimal.Types
open Minimal.Common
open Utils

let type_expr_testable = Alcotest.testable pp_type_expr ( = )
let type_info_testable = Alcotest.testable pp_type_info ( = )

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
      |> Typechk.expression StrMap.empty
      |> Alcotest.check type_expr_testable name expected)
    test_cases
;;

let test_record () =
  let types_list_before =
    Hashtbl.fold (fun _ info acc -> info :: acc) Minimal.Define.types []
  in
  do_phrase {| type Test = { mutable a: int } |};
  let types_list_after =
    Hashtbl.fold (fun _ info acc -> info :: acc) Minimal.Define.types []
  in
  let diff_list l1 l2 =
    let loop_over l' =
      let rec loop l =
        match l with
        | [] -> []
        | hd :: tl ->
          if List.exists (fun x -> x = hd) l' then loop tl else hd :: loop tl
      in
      loop
    in
    loop_over l2 l1 @ loop_over l1 l2
  in
  Alcotest.(check @@ list type_info_testable)
    "After declaring record type"
    [ { ti_params = []
      ; ti_res = Tconstr ({ name = "Test"; index = 9 }, [])
      ; ti_kind =
          Krecord [ "a", Tconstr ({ name = "int"; index = 1 }, []), Mutable ]
      }
    ]
    (diff_list types_list_before types_list_after)
;;

let () =
  Alcotest.run
    "Typechk"
    [ "expression", [ Alcotest.test_case "const" `Quick test_constants ]
    ; "type declaration", [ Alcotest.test_case "record" `Quick test_record ]
    ]
;;
