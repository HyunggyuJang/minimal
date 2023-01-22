(* The lexer definition *)

{
(**) open Parser

(* For nested comments *)

var comment_depth = 0

(* The table of keywords *)

val keyword_table : (string, token) Hashtbl.t = Hashtbl.create 57 ;

List.iter (fn (str,tok) => Hashtbl.add keyword_table str tok)
  [ ("and", AND),
    ("begin", BEGIN),
    ("case", CASE),
    ("do", DO),
    ("downto", DOWNTO),
    ("else", ELSE),
    ("end", END),
(*    ("exception", EXCEPTION), *)
    ("fn", FN),
    ("for", FOR),
    ("fun", FUN),
(*    ("handle", HANDLE), *)
    ("hide", HIDE),
    ("if", IF),
    ("mutable", MUTABLE),
    ("of", OF),
    ("or", OR),
    ("then", THEN),
    ("to", TO),
    ("type", TYPE),
    ("val", VAL),
    ("var", VAR),
    ("while", WHILE),

    ("mod", INFIX3("mod"))
  ]

fun add_infix s =
  Hashtbl.add keyword_table s (INFIX2 s)

fun remove_infix s =
  Hashtbl.remove keyword_table s


(* To buffer string literals *)

val initial_string_buffer = String.create 256
var string_buff = initial_string_buffer
var string_index = 0

fun reset_string_buffer () =
  begin
    string_buff <- initial_string_buffer;
    string_index <- 0;
  end

fun store_string_char c =
  begin
    if string_index >= String.length string_buff then begin
      val new_buff = String.create (String.length string_buff * 2);
      String.blit string_buff 0 new_buff 0 (String.length string_buff);
      string_buff <- new_buff
    end;
    String.set string_buff string_index c;
    string_index <- string_index + 1
  end

fun get_stored_string () =
  begin
    val s = String.sub string_buff 0 string_index;
    string_buff <- initial_string_buffer;
    s
  end

(* To translate escape sequences *)

fun char_for_backslash c =
  case c of
    'n' => '\010'
  | 'r' => '\013'
  | 'b' => '\008'
  | 't' => '\009'
  | c   => c
  end

fun char_for_decimal_code lexbuf i =
  begin
    val c = 
      100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
       10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
            (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48);
    Char.chr (c land 0xFF)
  end

type error =
    Unterminated_string
  | Unterminated_comment
  | Illegal_character

exception Lexical_error of error * int * int

}

rule main = parse
    [' ' '\010' '\013' '\009' '\012'] +
      { main lexbuf }
  | ['A'-'Z' 'a'-'z' '\160'-'\255' ]
    ( '_' ? ['A'-'Z' 'a'-'z' '\160'-'\255' ''' '0'-'9' ] ) *
      { begin
	  val s = Lexing.lexeme lexbuf;
          begin Hashtbl.find keyword_table s
          handle Not_found => IDENT s end
        end }
  | ['0'-'9']+
    | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    | '0' ['o' 'O'] ['0'-'7']+
    | '0' ['b' 'B'] ['0'-'1']+
      { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOAT (float_of_string(Lexing.lexeme lexbuf)) }
  | "\""
      { begin
	  reset_string_buffer();
          val string_start = Lexing.lexeme_start lexbuf;
          begin
            string lexbuf
          handle Lexical_error(Unterminated_string, _, string_end) =>
              raise
		(Lexical_error(Unterminated_string, string_start, string_end))
          end;
          lexbuf.Lexing.lex_start_pos <-
	    string_start - lexbuf.Lexing.lex_abs_pos;
          STRING (get_stored_string())
        end }
  | "'"
      { begin
	  val char_start = Lexing.lexeme_start lexbuf;
          val token = char_or_var lexbuf;
          lexbuf.Lexing.lex_start_pos <-
	    char_start - lexbuf.Lexing.lex_abs_pos;
          token
        end }
  | "(*"
      { begin
          val comment_start = Lexing.lexeme_start lexbuf;
          comment_depth <- 1;
          begin
            comment lexbuf
          handle Lexical_error(Unterminated_comment, _, comment_end) =>
            raise(Lexical_error(Unterminated_comment,
                              comment_start, comment_end))
          end;
          main lexbuf
        end }
  | "#" { SHARP }
  | "&" { AMPERSAND }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "*" { STAR }
  | "," { COMMA }
  | "->" { MINUSGREATER }
  | "=>" { EQUALGREATER }
  | "." { DOT }
  | ":" { COLON }
  | "::" { COLONCOLON }
  | ";" { SEMI }
  | "<-" { LESSMINUS }
  | "=" { EQUAL }
  | "==" { EQUALEQUAL }
  | "[" { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "]" { RBRACKET }
  | "_" { UNDERSCORE }
  | "{" { LBRACE }
  | "|" { BAR }
  | "|]" { BARRBRACKET }
  | "}" { RBRACE }

  | "-"     { SUBTRACTIVE "-" }
  | "-."    { SUBTRACTIVE "-." }

  | "!="
  | "<>"
  | ['<' '>'] '=' ?
            { INFIX0(Lexing.lexeme lexbuf) }
  | ['@' '^']
            { INFIX1(Lexing.lexeme lexbuf) }
  | '+' '.' ?
            { INFIX2(Lexing.lexeme lexbuf) }
  | "**"
            { INFIX4(Lexing.lexeme lexbuf) }
  | [ '*' '/' ] '.' ?
            { INFIX3(Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _
      { raise (Lexical_error(Illegal_character,
                   Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

and comment = parse
    "(*"
      { begin comment_depth <- comment_depth + 1; comment lexbuf end }
  | "*)"
      { begin
          comment_depth <- comment_depth - 1;
          if comment_depth > 0 then comment lexbuf
        end }
  | "\""
      { begin
          reset_string_buffer();
          val string_start = Lexing.lexeme_start lexbuf;
          begin
            string lexbuf
          handle Lexical_error(Unterminated_string, _, string_end) =>
            raise(Lexical_error(Unterminated_string, string_start, string_end))
          end;
          comment lexbuf
        end}
  | "'\"'"
      { comment lexbuf }
  | eof
      { raise(Lexical_error
                (Unterminated_comment, 0, Lexing.lexeme_start lexbuf)) }
  | _
      { comment lexbuf }

and char_or_var = parse
    [^ '\\' '''] "'"
      { CHAR (Lexing.lexeme_char lexbuf 0) }
  | '\\' ['\\' ''' 'n' 't' 'b' 'r'] "'"
      { CHAR (char_for_backslash (Lexing.lexeme_char lexbuf 1)) }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR (char_for_decimal_code lexbuf 1) }
  | ['A'-'Z' 'a'-'z' '\160'-'\255' ]
    ( '_' ? ['A'-'Z' 'a'-'z' '\160'-'\255' ''' '0'-'9' ] ) *
      { QUOTED (Lexing.lexeme lexbuf) }

and string = parse
    '"' (*"'"'*)
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { begin
          store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
          string lexbuf
        end }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { begin
          store_string_char(char_for_decimal_code lexbuf 1);
          string lexbuf
        end }
  | eof
      { raise (Lexical_error
                (Unterminated_string, 0, Lexing.lexeme_start lexbuf)) }
  | _
      { begin
          store_string_char(Lexing.lexeme_char lexbuf 0);
          string lexbuf
        end }
