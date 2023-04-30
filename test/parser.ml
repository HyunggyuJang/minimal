open Minimal.Syntax
open Utils

(* Generated by menhir *)
let print_token : Parser.token -> string =
 fun _tok ->
  match _tok with
  | AMPERSAND -> "AMPERSAND"
  | AND -> "AND"
  | BAR -> "BAR"
  | BARRBRACKET -> "BARRBRACKET"
  | BEGIN -> "BEGIN"
  | CASE -> "CASE"
  | CHAR _ -> "CHAR"
  | COLON -> "COLON"
  | COLONCOLON -> "COLONCOLON"
  | COMMA -> "COMMA"
  | DO -> "DO"
  | DOT -> "DOT"
  | DOWNTO -> "DOWNTO"
  | ELSE -> "ELSE"
  | END -> "END"
  | EOF -> "EOF"
  | EQUAL -> "EQUAL"
  | EQUALEQUAL -> "EQUALEQUAL"
  | EQUALGREATER -> "EQUALGREATER"
  | FLOAT f -> "FLOAT " ^ string_of_float f
  | FN -> "FN"
  | FOR -> "FOR"
  | FUN -> "FUN"
  | HIDE -> "HIDE"
  | IDENT s -> "IDENT " ^ s
  | IF -> "IF"
  | IN -> "IN"
  | INFIX0 _ -> "INFIX0"
  | INFIX1 _ -> "INFIX1"
  | INFIX2 _ -> "INFIX2"
  | INFIX3 _ -> "INFIX3"
  | INFIX4 _ -> "INFIX4"
  | INT i -> "INT " ^ string_of_int i
  | LBRACE -> "LBRACE"
  | LBRACKET -> "LBRACKET"
  | LBRACKETBAR -> "LBRACKETBAR"
  | LESSMINUS -> "LESSMINUS"
  | LPAREN -> "LPAREN"
  | MINUSGREATER -> "MINUSGREATER"
  | MUTABLE -> "MUTABLE"
  | OF -> "OF"
  | OR -> "OR"
  | PREFIX _ -> "PREFIX"
  | QUOTED s -> "QUOTED " ^ s
  | RBRACE -> "RBRACE"
  | RBRACKET -> "RBRACKET"
  | RPAREN -> "RPAREN"
  | SEMI -> "SEMI"
  | STAR -> "STAR"
  | STRING s -> "STRING " ^ s
  | SUBTRACTIVE _ -> "SUBTRACTIVE"
  | THEN -> "THEN"
  | TO -> "TO"
  | TYPE -> "TYPE"
  | UNDERSCORE -> "UNDERSCORE"
  | VAL -> "VAL"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
;;

let expression_testable = Alcotest.testable pp_expression equal_expression

let test_tokens () =
  let open Lexer in
  let success_table =
    [ "var", "VAR", "var"
    ; "string", "STRING test", {|"test"|}
    ; "char", "CHAR", {|'a'|}
    ; "ident", "IDENT ident", "ident"
    ; "number", "INT 237", "237"
    ; "negative", "SUBTRACTIVE", "-237"
    ; "float", "FLOAT 23.8", "23.8"
    ; "true", "IDENT true", "true"
    ; "infix0: less than", "INFIX0", "<"
    ; "infix0: greater than", "INFIX0", ">"
    ; "infix0: less than or equals to", "INFIX0", "<="
    ; "equal", "EQUAL", "="
    ; "quoted", "QUOTED hi", "'hi"
    ]
  in
  let failure_table =
    [ ( "unterminated string"
      , Lexical_error (Unterminated_string, 0, 13)
      , {|"unterminated|} )
    ; ( "unterminated comment"
      , Lexical_error (Unterminated_comment, 0, 15)
      , "(* unterminated" )
    ]
  in
  List.iter
    (fun (name, expected, input) ->
      Lexing.from_string input
      |> main
      |> print_token
      |> Alcotest.(check string) name expected)
    success_table;
  List.iter
    (fun (name, exn, input) ->
      Alcotest.check_raises name exn (fun () ->
        let _ = Lexing.from_string input |> main in
        ()))
    failure_table
;;

let make_expression desc = { se_desc = desc; se_loc = dummy_loc }

let test_simple_expr () =
  let success_table =
    [ "ident", SEid "ident", "ident"
    ; ( "string"
      , SEarray
          (List.map
             (fun c -> make_expression @@ SEconst (Cchar c))
             (Array.to_list (Minimal.Misc.array_of_string "string")))
      , {|"string"|} )
    ; "constant, int", SEconst (Cint 8), "8"
    ; "constant, char", SEconst (Cchar (int_of_char 'a')), {|'a'|}
    ; "constant, float", SEconst (Cfloat 2.8), "2.8"
    ; ( "tuple"
      , SEtuple (List.map make_expression [ SEconst (Cint 0); SEid "true" ])
      , "(0,true)" )
    ; "infx", SEid "<", "(<)"
    ; ( "array"
      , SEarray
          (List.map
             (fun e -> make_expression e)
             [ SEconst (Cfloat 2.8); SEid "true" ])
      , "[|2.8, true|]" )
    ; ( "record"
      , SErecord [ "test", make_expression @@ SEid "test" ]
      , "{ test = test }" )
    ; ( "field access"
      , SEgetfield (make_expression @@ SEid "test", "test")
      , "test.test" )
    ; ( "array access"
      , SEapply
          ( make_expression @@ SEid "."
          , [ make_expression @@ SEid "test"
            ; make_expression @@ SEconst (Cint 8)
            ] )
      , "test.[8]" )
    ; ( "function"
      , SEfunct
          ( [ { sp_desc = SPid "x"; sp_loc = dummy_loc } ]
          , make_expression @@ SEid "x" )
      , "fn x => x" )
    ]
  in
  List.iter
    (fun (name, expected, input) ->
      parse_exp input
      |> Alcotest.check expression_testable name (make_expression expected))
    success_table
;;

let test_expr () =
  let success_table =
    [ "negation has the highest priority", "(-4) ** 8", "-4 ** 8"
    ; ( "exponent has higher priority than others"
      , "2 + (3 * (2 ** 4)) < 8"
      , "2 + 3 * 2 ** 4 < 8" )
    ; ( "identical operator's priority equals to equal operator"
      , "(0 == 3) = 4"
      , "0 == 3 = 4" )
    ; "math operators are left associative", "(2 + 4) - 8", "2 + 4 - 8"
    ]
  in
  List.iter
    (fun (name, expected, input) ->
      Alcotest.check
        expression_testable
        name
        (parse_exp expected)
        (parse_exp input))
    success_table
;;

let () =
  Alcotest.run
    "Parser"
    [ ( "parse"
      , [ Alcotest.test_case "tokens" `Quick test_tokens
        ; Alcotest.test_case "simple expressions" `Quick test_simple_expr
        ; Alcotest.test_case "expressions" `Quick test_expr
        ] )
    ]
;;
