exception Line_too_long

let prompt = ref ""

type lineno = Ln of int * linetxt

and linetxt =
  | Txt of string * lineno
  | Fstln

let lineno (Ln (n, _)) = n

let feed_lexer ic echo lines buffer n =
  try
    if echo
    then (
      print_string !prompt;
      Format.print_flush ());
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

let minimal () =
  Sys.catch_break true;
  Format.open_hbox ();
  Format.printf ">\tMiniMAL version 1.0 (%s) on Objective Caml@.@." "UTF-8"
;;

minimal ()
