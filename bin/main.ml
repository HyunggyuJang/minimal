(* $Id: toploop.ml,v 1.24 2018/06/26 01:30:27 garrigue Exp $ *)

open Minimal
open Misc
open Common
open Syntax
open Types
open Define
open Typechk
open Compile
open Printer
open Builtins
open Format
open Lexing

let rec diff_list l1 l2 =
  if l1 == l2 then [] else List.hd l1 :: diff_list (List.tl l1) l2
;;

exception Line_too_long

let prompt = ref ""

type lineno = Ln of int * linetxt

and linetxt =
  | Txt of string * lineno
  | Fstln

let linetxt (Ln (_, txt)) = txt
let lineno (Ln (n, _)) = n

let feed_lexer ic echo lines buffer n =
  try
    if echo
    then (
      print_string !prompt;
      print_flush ());
    prompt := "  ";
    let s = input_line ic in
    let len = String.length s in
    if len > n - 1 then raise Line_too_long;
    Bytes.blit_string s 0 buffer 0 len;
    Bytes.set buffer len '\n';
    lines := Ln (len + 1 + lineno !lines, Txt (s, !lines));
    len + 1
  with
  | End_of_file -> 0
;;

let rec linecnt = function
  | Fstln -> 1
  | Txt (_, Ln (_, lines)) -> 1 + linecnt lines
;;

let rec get_line n = function
  | Fstln -> 0, 0, ""
  | Txt (s, Ln (start, rest)) ->
    if start > n then get_line n rest else linecnt rest, start, s
;;

let skip_back s pos1 pos0 =
  let pos' = ref pos0 in
  while !pos' >= pos1 && (s.[!pos'] = ' ' || s.[!pos'] = '\t') do
    decr pos'
  done;
  !pos'

and skip_forward s pos0 pos2 =
  let pos' = ref pos0 in
  while !pos' < pos2 && (s.[!pos'] = ' ' || s.[!pos'] = '\t') do
    incr pos'
  done;
  !pos'
;;

let output_location filename lines oc loc =
  if filename <> ""
  then fprintf oc "@[In \"%s\", line " filename
  else fprintf oc "@[Line ";
  let line, offset, s = get_line loc.first.pos_cnum lines in
  let line', offset', s' = get_line loc.last.pos_cnum lines in
  if line = line'
  then (
    let pos = min (String.length s) (loc.first.pos_cnum - offset) in
    let len =
      min (String.length s - pos) (loc.last.pos_cnum - loc.first.pos_cnum)
    in
    fprintf
      oc
      "%d, char %d-%d:@ \"%s\"@]"
      line
      (loc.first.pos_cnum - offset)
      (loc.last.pos_cnum - offset - 1)
      (String.sub s pos len))
  else (
    let pos1 = loc.first.pos_cnum - offset
    and pos2 = loc.last.pos_cnum - offset' in
    let s1 =
      let pos' = skip_back s pos1 (String.length s - 1) in
      let pos' =
        if pos' - pos1 < 20
        then pos'
        else (
          try skip_back s pos1 (String.rindex_from s (pos1 + 19) ' ') with
          | Not_found -> skip_back s pos1 (pos1 + 19))
      in
      String.sub s pos1 (pos' + 1 - pos1)
    and s2 =
      let pos' = skip_forward s' 0 pos2 in
      let pos' =
        if pos2 - pos' <= 20
        then pos'
        else (
          let pos0 = min (pos2 - 20) (String.length s') in
          try skip_forward s' (String.index_from s' pos0 ' ') pos2 with
          | Not_found -> skip_forward s' pos0 pos2)
      in
      String.sub s' pos' (pos2 - pos')
    in
    fprintf
      oc
      "%d, char %d to line %d, char %d:@ \"%s ... %s\"@]"
      line
      pos1
      line'
      (pos2 - 1)
      s1
      s2)
;;

exception Toplevel

let verbose = ref true

let do_phrase ph =
  try
    match ph with
    | { sc_desc = SEexpr e; _ } ->
      let ty = type_expression !values e in
      let ue = Translate.expression !global_idents e in
      let obj = compile_expression ue () in
      flush stdout;
      flush stderr;
      if !verbose
      then (
        open_hovbox 1;
        print_string "- : ";
        print_scheme ty;
        print_string " =";
        print_space ();
        print_value ty obj;
        close_box ();
        print_newline ())
    | { sc_desc = STtype dl; _ } ->
      add_typedef ph.sc_loc dl;
      if !verbose
      then List.iter (fun td -> printf "type %s defined.@." td.sd_name) dl
    | { sc_desc = SThide s; _ } ->
      hide_type ph.sc_loc s;
      printf "type %s is now abstract.@." s
    | cmd ->
      let new_values = type_command !values cmd in
      let ucmds, new_idents = Translate.command !global_idents cmd in
      let idents = diff_list new_idents !global_idents in
      (* Some values must be preallocated *)
      List.iter
        (fun id -> add_value id (Obj.repr ()) (StrMap.find id.name new_values))
        (prealloc_idents ucmds);
      let objs = compile_commands ucmds (List.map snd idents) in
      flush stdout;
      flush stderr;
      (* Only add values that where not preallocated *)
      List.iter2
        (fun (s, id) obj ->
          try
            let _ = IdMap.find id !global_env in
            ()
          with
          | Not_found -> add_value id obj (StrMap.find s new_values))
        idents
        objs;
      if !verbose
      then
        List.iter2
          (fun (s, _) obj ->
            let info = StrMap.find s new_values in
            open_hovbox 1;
            print_string (if info.vi_access = Mutable then "var " else "val ");
            print_string (s ^ " :");
            print_space ();
            print_scheme info.vi_type;
            print_string " =";
            print_space ();
            print_value info.vi_type obj;
            close_box ();
            print_newline ())
          idents
          objs
  with
  | exn -> if handle exn then raise Toplevel else raise exn
;;

let report_error lexbuf filename lines exn =
  (match exn with
  | Parser.Error ->
    printf
      "> %a\n  Syntax error.@."
      (output_location filename lines)
      { first = lexeme_start_p lexbuf; last = lexeme_end_p lexbuf }
  | Lexer.Lexical_error (_, start, stop) ->
    printf
      "> %a\n  Lexical error@."
      (output_location filename lines)
      { first = { dummy_pos with pos_cnum = start }
      ; last = { dummy_pos with pos_cnum = stop }
      }
  | Type_error (err, loc) ->
    printf "> %a  " (output_location filename lines) loc;
    report_error err;
    printf ".@."
  | Line_too_long ->
    printf "> Too long line in \"%s\", line %d.@." filename (linecnt lines)
  | Sys.Break -> prerr_endline "Interrupted!"
  | _ -> raise exn);
  raise Toplevel
;;

let loaded_files = ref []

let use require s =
  let name, ic =
    let name = string_of_array s in
    try
      let name = name ^ ".mal" in
      name, open_in name
    with
    | Sys_error _ ->
      (try name, open_in name with
      | Sys_error _ ->
        printf "> Could not open \"%s\".@." name;
        raise Toplevel)
  in
  if require && List.mem name !loaded_files
  then ()
  else (
    let lines = ref (Ln (0, Fstln)) in
    let lexbuf = Lexing.from_function (feed_lexer ic false lines) in
    try
      Lexer.skip_sb lexbuf;
      while true do
        List.iter do_phrase (Parser.phrase Lexer.main lexbuf)
      done
    with
    | exn ->
      close_in ic;
      (match exn with
      | End_of_file ->
        if not (List.mem name !loaded_files)
        then loaded_files := name :: !loaded_files
      | _ -> report_error lexbuf name (linetxt !lines) exn))
;;

builtins
  := [ "use", Obj.repr (use false), arr string unit
     ; "include", Obj.repr (use false), arr string unit
     ; "require", Obj.repr (use true), arr string unit
     ]
     @ !builtins

let minimal () =
  Sys.catch_break true;
  open_hbox ();
  Format.printf ">\tMiniMAL version 1.0 (%s) on Objective Caml@.@." charset;
  let eof_count = ref 0 in
  (* Windows hack, Ctrl-C causes eof *)
  try
    while true do
      try
        Gc.full_major ();
        let lines = ref (Ln (0, Fstln)) in
        let lexbuf = Lexing.from_function (feed_lexer stdin true lines) in
        try
          while true do
            flush stdout;
            flush stderr;
            let ok = ref true in
            for i = lexbuf.lex_curr_pos to lexbuf.lex_buffer_len - 1 do
              let c = Bytes.get lexbuf.lex_buffer i in
              if c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n'
              then ok := false
            done;
            if !ok
            then (
              prompt := "# ";
              lines := Ln (lineno !lines, Fstln));
            List.iter do_phrase (Parser.phrase Lexer.main lexbuf)
          done
        with
        | exn ->
          (try report_error lexbuf "" (linetxt !lines) exn with
          | Toplevel -> ())
      with
      | Sys.Break ->
        eof_count := 0;
        prerr_endline "Interrupted!"
      | End_of_file when !eof_count < 5 && Sys.os_type = "Win32" ->
        incr eof_count
    done
  with
  | End_of_file -> close_box ()
;;

add_builtins ()

let main () =
  let argc = Array.length Sys.argv in
  if argc = 1
  then minimal ()
  else if argc = 2
  then (
    verbose := false;
    try use false (array_of_string Sys.argv.(1)) with
    | Toplevel -> ())
  else prerr_endline "Usage: minimal [file.mal]"
;;

main ()
