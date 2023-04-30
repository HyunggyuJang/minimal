module Parser = Minimal.Parser
module Lexer = Minimal.Lexer
open Minimal.Syntax

exception Utils

let parse s = Lexing.from_string s |> Parser.phrase Lexer.main

let parse_exp s =
  parse s |> fun e ->
  match (List.hd e).sc_desc with
  | SEexpr exp -> exp
  | _ -> assert false
;;

let rec diff_list l1 l2 =
  if l1 == l2 then [] else List.hd l1 :: diff_list (List.tl l1) l2
;;

let do_phrase s =
  let open Minimal in
  let open Typechk in
  let open Define in
  let open Common in
  let open Compile in
  try
    match List.hd (parse s) with
    | { sc_desc = STtype dl; sc_loc } -> add_typedef sc_loc dl
    | cmd ->
      let new_values = type_command !values cmd in
      let ucmds, new_idents = Translate.command !global_idents cmd in
      let idents = diff_list new_idents !global_idents in
      (* Some values must be preallocated *)
      List.iter
        (fun id -> add_value id (Obj.repr ()) (StrMap.find id.name new_values))
        (prealloc_idents ucmds);
      let objs = compile_commands ucmds (List.map snd idents) in
      (* Only add values that where not preallocated *)
      List.iter2
        (fun (s, id) obj ->
          try
            let _ = IdMap.find id !global_env in
            ()
          with
          | Not_found -> add_value id obj (StrMap.find s new_values))
        idents
        objs
  with
  | exn -> if Minimal.Builtins.handle exn then raise Utils else raise exn
;;

let dummy_loc = { first = Lexing.dummy_pos; last = Lexing.dummy_pos }
