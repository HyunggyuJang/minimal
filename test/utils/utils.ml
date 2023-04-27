module Parser = Minimal.Parser
module Lexer = Minimal.Lexer
open Minimal.Syntax

let parse s =
  Lexing.from_string s |> Parser.phrase Lexer.main |> fun e ->
  match (List.hd e).sc_desc with
  | SEexpr exp -> exp
  | _ -> assert false
;;
