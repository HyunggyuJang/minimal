/* The parser definition */

%{
open Lexing
open Misc
open Common
open Syntax

fun get_location () = { first = symbol_start () , last = symbol_end () }

fun make_typ desc =
  { st_desc = desc , st_loc = get_location () }
and make_pat desc =
  { sp_desc = desc , sp_loc = get_location () }
and make_cmd desc =
  { sc_desc = desc , sc_loc = get_location () }
and make_expr desc =
  { se_desc = desc , se_loc = get_location () }
and make_typedef name params kind =
  { sd_name = name , sd_params = params , sd_kind = kind ,
    sd_loc = get_location () }
and make_ident s =
  { se_desc = SEid s , se_loc = get_location () }

fun make_string s =
  make_expr (SEarray (List.map
			(fn c => { se_desc = SEconst (Cchar c),
				   se_loc = get_location () })
			(Array.to_list (array_of_string s))))
and make_unop s e =
  make_expr (SEapply (make_ident s, [e]))
and make_binop s e1 e2 =
  make_expr (SEapply (make_ident s, [e1, e2]))
and make_ternop s e1 e2 e3 =
  make_expr (SEapply (make_ident s, [e1, e2, e3]))

fun make_pat_string s =
  make_pat (SParray (List.map
		       (fn c => { sp_desc = SPconst (Cchar c) ,
				  sp_loc = get_location () })
		       (Array.to_list (array_of_string s))))
and make_cons_pat a l =
  make_pat (SPconstr ("::", make_pat (SPtuple [a, l])))

%}

/* Tokens */

/* Identifiers, prefixes, infixes */
%token <string> IDENT
%token <string> QUOTED
%token <string> PREFIX
%token <string> INFIX0
%token <string> INFIX1
%token <string> INFIX2
%token <string> SUBTRACTIVE
%token <string> INFIX3
%token <string> INFIX4
/* Literals */
%token <int> INT
%token <char> CHAR
%token <float> FLOAT
%token <string> STRING
/* The end-of-file marker */
%token EOF
/* Special symbols */
%token EQUAL          /* "=" */
%token EQUALEQUAL     /* "==" */
%token SHARP          /* "#" */
%token AMPERSAND      /* "&" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token STAR           /* "*" */
%token COMMA          /* "," */
%token MINUSGREATER   /* "->" */
%token EQUALGREATER   /* "=>" */
%token DOT            /* "." */
%token COLON          /* ":" */
%token COLONCOLON     /* "::" */
%token SEMI           /* ";" */
%token LBRACKET       /* "[" */
%token LBRACKETBAR    /* "[|" */
%token LESSMINUS      /* "<-" */
%token RBRACKET       /* "]" */
%token UNDERSCORE     /* "_" */
%token LBRACE         /* "{" */
%token BAR            /* "|" */
%token BARRBRACKET    /* "|]" */
%token RBRACE         /* "}" */
%token UNDERSCORE     /* "_" */
/* Keywords */
%token AND            /* "and" */
%token BEGIN          /* "begin" */
%token CASE	      /* "case" */
%token DO             /* "do" */
%token DOWNTO         /* "downto" */
%token ELSE           /* "else" */
%token END            /* "end" */
/* %token EXCEPTION      /* "exception" */
%token FOR            /* "for" */
%token FN	      /* "fn" */
%token FUN            /* "fun" */
%token HIDE	      /* "hide" */
%token IF             /* "if" */
%token IN             /* "in" */
%token MUTABLE        /* "mutable" */
%token OF             /* "of" */
%token OR             /* "or" */
%token THEN           /* "then" */
%token TO             /* "to" */
%token TYPE           /* "type" */
%token VAL	      /* "val" */
%token VAR	      /* "val" */
%token WHILE          /* "while" */

/* Precedences and associativities. Lower precedences first. */

%right MINUSGREATER EQUALGREATER
%right SEMI
%right AND
%right prec_list
%right prec_if
%right LESSMINUS
%left  BAR
%left  COMMA
%left  OR
%left  AMPERSAND
%left  INFIX0 EQUAL EQUALEQUAL	        /* comparisons */
%right INFIX1                           /* concatenations */
%right COLONCOLON                       /* cons */
%left  INFIX2 SUBTRACTIVE               /* additives, subtractives */
%left  STAR INFIX3                      /* multiplicatives */
%right INFIX4                           /* exponentiations */
%right prec_uminus
%right prec_app
%left  DOT

/* Entry points */

%start phrase
%type <Syntax.command list> phrase

/*
%start Type_expr
%type <syntax__type_expr> Type_expr
%start Pattern
%type <syntax__pattern> Pattern
%start Expression
%type <syntax__expression> Expression
%start Command
%type <syntax__command> Command
%start Typedef
%type <syntax__typedef> Typedef
*/

%%

/* One toplevel phrase */

phrase :
        top_command_list SEMI
	  { List.rev $1 }
      | top_command_list EOF
	  { List.rev $1 }
      |	EOF
	  { raise End_of_file }
;

command :
	expression
	  { make_cmd (SEexpr $1) }
      |	definition
	  { $1 }
;

definition :
        VAL value_def
	  { make_cmd (SEval $2) }
      |	FUN function_def
	  { make_cmd (SEfun $2) }
      |	VAR var_def
	  { make_cmd (SEvar $2) }
      |	TYPE typedef_list
	  { make_cmd (STtype $2) }
      |	HIDE IDENT
	  { make_cmd (SThide $2) }
;

command_list :
	command_list SEMI command
	  { $3 :: $1 }
      |	command_list definition
	  { $2 :: $1 }
      |	command
	  { [$1] }
;
top_command_list :
      	top_command_list definition
	  { $2 :: $1 }
      |	command
	  { [$1] }
;

value_def :
	pattern expression_def AND value_def
	  { ($1,$2) :: $4 }
      |	pattern expression_def
	  { [($1,$2)] }
;

function_def :
	IDENT equation AND function_def
	  { ($1,$2) :: $4 }
      |	IDENT equation
	  { [($1,$2)] }
;

var_def :
	IDENT expression_def AND var_def
	  { ($1,$2) :: $4 }
      |	IDENT expression_def
	  { [($1,$2)] }
;

equation :
	simple_pattern_list expression_def
	  { make_expr (SEfunct ($1, $2)) }
;

expression_def :
	COLON type_expr EQUAL expression
	  { make_expr (SEtype ($4,$2)) }
      |	EQUAL expression
	  { $2 }
;

expression :
	simple_expr
	  { $1 }
      | SUBTRACTIVE expression  %prec prec_uminus
          { make_unop (if $1 = "-" then "~" else "~.") $2 }
      | expression INFIX4 expression
          { make_binop $2 $1 $3 }
      | expression INFIX3 expression
          { make_binop $2 $1 $3 }
      | expression INFIX2 expression
          { make_binop $2 $1 $3 }
      | expression SUBTRACTIVE expression
          { make_binop $2 $1 $3 }
      | expression INFIX1 expression
          { make_binop $2 $1 $3 }
      | expression INFIX0 expression
          { make_binop $2 $1 $3 }
      | expression EQUAL expression
          { make_binop "=" $1 $3 }
      | expression EQUALEQUAL expression
          { make_binop "==" $1 $3 }
      | expression STAR expression
          { make_binop "*" $1 $3 }
      |	expression AMPERSAND expression
	  { make_binop "&" $1 $3 }
      |	expression OR expression
	  { make_binop "or" $1 $3 }
      |	expression COLONCOLON expression
	  { make_unop "::" (make_expr (SEtuple [$1,$3])) }
      | simple_expr simple_expr_list   %prec prec_app
          { make_expr (SEapply ($1, $2)) }
      |	FN simple_pattern_list EQUALGREATER expression
	  { make_expr (SEfunct ($2, $4)) }
      |	BEGIN expr_cmd_list END
	  { $2 }
      |	CASE expression OF opt_bar matching END
	  { make_expr (SEcase ($2, $5)) }
      |	IF expression THEN expression ELSE expression
	  { make_expr (SEifthenelse ($2, $4, $6)) }
      |	IF expression THEN expression
	  { make_expr (SEifthenelse ($2, $4, make_expr (SEtuple []))) }
      |	IDENT LESSMINUS expression
	  { make_expr (SEset ($1, $3)) }
      |	simple_expr DOT LBRACKET expression RBRACKET LESSMINUS expression
	  { make_ternop ".<-" $1 $4 $7 }
      |	simple_expr DOT IDENT LESSMINUS expression
	  { make_expr (SEsetfield ($1, $3, $5)) }
      |	FOR IDENT EQUAL expression dir_flag expression DO expression
	  { make_expr (SEfor ($2, $4, $5, $6, $8)) }
      |	WHILE expression DO expression
	  { make_expr (SEwhile ($2, $4)) }
;

expression_comma_list :
	expr_may_type COMMA expression_comma_list
	  { $1 :: $3 }
      |	expr_may_type
	  { [$1] }
      |	/* empty */
	  { [] }
;

expr_may_type :
	expression COLON type_expr
	  { make_expr (SEtype ($1, $3)) }
      |	expression
	  { $1 }
;

label_expr_list :
	IDENT EQUAL expression COMMA label_expr_list
	  { ($1,$3) :: $5 }
      |	IDENT EQUAL expression
	  { [($1,$3)] }
;

dir_flag :
	TO	{ Upto }
      |	DOWNTO	{ Downto }
;

simple_expr :
	IDENT
	  { make_expr (SEid $1) }
      |	STRING
	  { make_string $1 }
      |	constant
	  { make_expr (SEconst $1) }
      |	LPAREN expression_comma_list RPAREN
	  { case $2 of [e] => e
	    | l => make_expr (SEtuple l)
	    end }
      |	LPAREN infx RPAREN
	  { make_ident $2 }
      |	LBRACKETBAR expression_comma_list BARRBRACKET
	  { make_expr (SEarray $2) }
      |	LBRACKET expression_comma_list RBRACKET
	  { List.fold_right
	      (fn a l => make_unop "::" (make_expr (SEtuple [a,l])))
	      $2 (make_ident "[]") }
      |	LBRACE label_expr_list RBRACE
	  { make_expr (SErecord $2) }
      |	simple_expr DOT IDENT
	  { make_expr (SEgetfield ($1, $3)) }
      |	simple_expr DOT LBRACKET expression RBRACKET
	  { make_binop "." $1 $4 }
;

simple_expr_list :
        simple_expr simple_expr_list
          { $1 :: $2 }
      | simple_expr
          { [$1] }
;

expr_cmd_list :
	command_list
	  { make_expr (SEseq (List.rev $1)) }
      |	command_list SEMI
	  { make_expr (SEseq (List.rev $1)) }
;

opt_bar :
	/* Empty */			{ () }
      |	BAR				{ () }
;
matching :
	pattern EQUALGREATER expr_cmd_list BAR matching
	  { ($1,$3) :: $5 }
      |	pattern EQUALGREATER expr_cmd_list
	  { [($1,$3)] }
;

pattern :
	simple_pattern
	  { $1 }
      |	IDENT simple_pattern		%prec prec_app
	  { make_pat (SPconstr ($1, $2)) }
      |	pattern COLONCOLON pattern
	  { make_cons_pat $1 $3 }
;

label_pattern_list :
	IDENT EQUAL pattern COMMA label_pattern_list
	  { ($1,$3) :: $5 }
      |	IDENT EQUAL pattern
	  { [($1,$3)] }
;

pattern_comma_list :
	pattern_may_type COMMA pattern_comma_list
	  { $1 :: $3 }
      |	pattern_may_type
	  { [$1] }
      |	/* empty */
	  { [] }
;

pattern_may_type :
	pattern COLON type_expr
	  { make_pat (SPtype ($1, $3)) }
      |	pattern
	  { $1 }
;

simple_pattern :
	IDENT
	  { make_pat (SPid $1) }
      |	UNDERSCORE
	  { make_pat SPany }
      |	STRING
	  { make_pat_string $1 }
      |	constant
	  { make_pat (SPconst $1) }
      |	SUBTRACTIVE INT
	  { make_pat (SPconst(Cint(- $2))) }
      |	SUBTRACTIVE FLOAT
	  { make_pat (SPconst(Cfloat(-. $2))) }
      |	LPAREN pattern_comma_list RPAREN
	  { case $2 of [pat] => pat
	    | l => make_pat (SPtuple l)
	    end }
      |	LBRACKET pattern_comma_list RBRACKET
	  { List.fold_right make_cons_pat
	      $2 { sp_desc = SPid "[]" , sp_loc = get_location () } }
      |	LBRACKETBAR pattern_comma_list BARRBRACKET
	  { make_pat (SParray $2) }
      |	LBRACE label_pattern_list RBRACE
	  { make_pat (SPrecord $2) }
;

simple_pattern_list :
        simple_pattern simple_pattern_list
          { $1 :: $2 }
      | simple_pattern
          { [$1] }
;

constant :
	INT	{ Cint $1 }
      |	CHAR	{ Cchar $1 }
      |	FLOAT	{ Cfloat $1 }
;

infx :
        INFIX0          { $1 }
      | INFIX1          { $1 }    | INFIX2        { $1 }
      | INFIX3          { $1 }    | INFIX4        { $1 }
      | STAR            { "*" }   | COLONCOLON    { "::" }
      | EQUAL           { "=" }   | SUBTRACTIVE     { $1 }
      | AMPERSAND       { "&" }   | OR              { "or" }
      |	EQUALEQUAL	{ "==" }
;

/* Type expressions */

type_expr :
	type_star_list
          { case List.rev $1 of [ty] => ty
	    | l => make_typ(STtuple l)
	    end }
      | type_expr MINUSGREATER type_expr
          { make_typ(STarrow($1, $3)) }
;

simple_type :
        QUOTED
          { make_typ(STvar $1) }
      | IDENT
          { make_typ(STconstr($1, [])) }
      | simple_type IDENT
          { make_typ(STconstr($2, [$1])) }
      | LPAREN type_expr COMMA type_comma_list RPAREN IDENT
          { make_typ(STconstr($6, $2 :: $4)) }
      | LPAREN type_expr RPAREN
          { $2 }
;

type_star_list :
        type_star_list STAR simple_type
          { $3 :: $1 }
      | simple_type
          { [$1] }
;

type_comma_list :
        type_expr COMMA type_comma_list
          { $1 :: $3 }
      | type_expr
          { [$1] }
;

/* Declarations */

typedef_list :
        typedef AND typedef_list
          { $1 :: $3 }
      | typedef
          { [$1] }
;

constr_decl :
        constr1_decl BAR constr_decl
          { $1 :: $3 }
      | constr1_decl
          { [$1] }
;

label_decl :
        label1_decl COMMA label_decl
          { $1 :: $3 }
      | label1_decl
          { [$1] }
;

typedef :
        type_params IDENT type1_def
          { make_typedef $2 $1 $3 }
;

type1_def :
	EQUAL opt_bar constr_decl
          { SKvariant $3 }
      | EQUAL LBRACE label_decl RBRACE
          { SKrecord $3 }
      | EQUALEQUAL type_expr
          { SKabbrev $2 }
;

constr1_decl :
        IDENT OF type_star_list
          { ($1, List.rev $3) }
      | IDENT
          { ($1, []) }
;

label1_decl :
        mutable_option IDENT COLON type_expr
          { ($2, $4, $1) }
;

mutable_option :
        MUTABLE
          { Mutable }
      | /* epsilon */
          { Immutable }
;

type_params :
        LPAREN type_var_list RPAREN
          { $2 }
      | QUOTED
          { [$1] }
      |
          { [] }
;

type_var_list :
        QUOTED COMMA type_var_list
          { $1 :: $3 }
      | QUOTED
          { [$1] }
;

%%
