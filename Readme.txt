              MiniMAL   : a mini Meta Algorithmic Language

                        Revision 1.0

0 Preface

  MiniMAL is a small ML-like programming language, featuring
  functional and imperative features, and type inference.
  It is mainly intended for educative purposes, but the source code
  version is easily extendable, as the internal data representation is
  compatible with Objective Caml.

  Generally it is used interactively, from a dos or shell prompt:

        C:\MINIMAL> minimal
        >       MiniMAL version 1.0 (SJIS) on Objective Caml
        
        # fun f x = x+1;
        val f : int -> int = <fun>
        # f 3;
        - : int = 4

  You can also directly execute a MiniMAL program by passing it as
  argument.

  A few examples are included in this distribution:
    prelude.mal
       Some common utility functions, often used in other examples
       through `require "prelude";'
    variable.mal, stack.mal, queue.mal, btree.mal
       Some common data structures one may want to use in programs
    poly.mal, matrix.mal
       Some common mathematical structures
    int32.mal     32-bit integer operations (default is 31-bit)
    music.mal     A little piano
    graphics.mal  Utilities to draw function graphs
    turtle.mal    Turtle graphics, as in LOGO

  The rest of this text describes MiniMAL and its standard library.

I Syntax

  A MiniMAL program is a sequence of phrases entered at toplevel. A
  phrase is either a definition (def), an expression (expr) or a type
  definition (td), followed by ";".

Ia Definitions (def)
    fun f p1 .. pn {: t} = expr {and f' ...}    function (recursive)
    val p = expr {and p' ...}                   value (non recursive)
    var x = expr {and x' ...}                   variable (non recursive)

Ib Expressions (expr)
  Simple expressions
    x                                           variable
    x <- expr                                   assigment
    1 1.3  'a' "hello" true                     constant/constructor
    (expr, ..., expr)                           tuple
    [expr, ..., expr]                           list
    [|expr, ..., expr|]                         array
    {lab = expr, ..., lab = expr}               record
    expr.[expr]         expr.lab                array/record extraction
    expr.[expr] <- expr expr.lab <- expr        array/record assignment
    expr expr1 ... exprn                        application
    expr op expr                                infix application
    fn p1 .. pn => expr                         abstraction

  Structured expressions
    begin expr or def ; ... ; expr or def end   sequential block
    case expr of p1 => expr or def; ... {| p2 => ...} end
                                                case statement
    if expr then expr {else expr}               if statement
    for x = expr {to|downto} expr do expr       for statement
    while expr do expr                          while statement

    * note about begin and case: the semicolon is facultatory before
      definitions and after the last expression. It is only compulsory
      before expressions. Similarly you can chain several definitions
      in one phrase at toplevel, without using the semicolon.

Ic Patterns (p)
    x                                   variable
    1 'a' "hello"                       constant
    _                                   wild card    
    (p1, ..., pn)                       tuple
    c0  cn(p1, ..., pn)                 constructor (0-ary, n-ary)
    p1::p2      [p1, ..., pn]           list
    [|p1, ..., pn|]                     array
    {lab1 = p1, ..., labn = pn}         tuple

Id Types (t)
    'a                                  variable
    t1 * ... * tn                       tuple
    c0   'a c1  ('a1,..,'an) cn         constructor (0-ary, 1-ary, n-ary)
    t1 -> t2                            function

Remark : expressions and patterns can be annotated with types. Just
  write (expr : t) or (p : t). For definitions, writing .. : t = expr
  is equivalent to .. = (expr : t), but probably more readable.

Ie Type definitions (td)
    type ('a,..) cn = C1 {of t1} | .. | Ck {of tk}      variant type
    type ('a,..) cn = {{mutable} lab1: t1, ...}         record type
    type ('a,..) cn == t                                abbreviation

  In records, a field has to be declared mutable to use assignment.

  Type definitions can be made mutually recursive by using and for
  following definitions.

    hide cn

  This last command hides cn and all its associated constructors and
  labels, making this type abstract. You can use it as a very
  primitive form of type abstraction.

II Basic types

IIa Numeric values
  A number containing no dot nor exponential notation is an integer,
  of type int. E.g. -3 1 30 5372572. Integers are represented on 31
  bits.

  A number containing a dot or an exponential notation is a floating
  point number, of type float. E.g. 1. 1.0 1.3 -1.5e4. Floats are
  represented on 64 bits following IEEE conventions.

  No coercion is done between integers and floats.

IIb Characters
  For a single character, you write 'c', like in C. Their type is
  char. Special characters can be entered in escaped form: '\n' '\r'
  '\t' '\\' '\'' or the 3-digit ascii number '\NNN'. Normal characters
  may have up to 31bits, depending on the encoding, but escaped ones
  are only 8bit.

  Strings are written "sdhjs", again like in C, using the same
  notation for special characters, including "\"". The type is string,
  but this is only an abbreviation for char array. You can use all
  array operations on strings, including destructive assignment to an
  indexed character. As a result, strings are not constant in general.

  Both characters and strings can use local charsets. Minimal will
  look successively for the suffix of environment variables
  MINIMAL_LANG, LC_ALL, LANG and LC_CTYPE (in this order) to decide
  which encoding to use. Supported encodings are SJIS, EUC and UTF8,
  plus the special encoding NOCONV meaning that characters are 8bit,
  and no conversion should be done. The default encoding is SJIS,
  so you should set explicitly MINIMAL_LANG=noconv if you want no
  conversion.

IIc Booleans
  Boolean values true and false are predefined as type bool. They are
  expected by if and while statements. Their definition is equivalent
  to
        type bool = false | true

IId Tuples
  Tuples (a1,..,an) when ai has type ti, are typed t1 * .. * tn.
  The only way to extract a value from a tuple is through pettern
  matching, either in a definition or a case statement.

  A special case is the empty tuple, (), which is typed unit, and is
  returned when there is nothing else to return: for and while
  statements, empty else branch of an if statements, block containing
  only definitions ...

IIe Arrays
  Arrays [|...|] are sequences of values of homogeneous type. If
  contained values have type t, they their array has type "t
  array". Value in nth position (starting from 0) can be accessed by
  arr.[n], and modified by arr.[n] <- expr. Boundary checks are
  performed.

IIf Lists
  The algebraic type for lists is predeclared as

        type 'a list = [] | :: of 'a * 'a list

  Remark that :: (cons) is infix, and this may also be used in
  patterns. [a1, .., an] is equivalent to a1::..::an::[].

III Semantics

IIIa Values and variables
  Definitions fun/val and var behave differently in terms of
  accessibility.

  If a value was defined by fun or val, you can use it anywhere in its 
  scope, but cannot modify it.

  If it is a variable defined by var, you can only use it locally
  --i.e. you cannot use it in locally defined functions-- but you can
  modify it by assignment.

  This last feature is quite unusual in functional programming
  languages, and is the reason to call this language algorithmic.

IIIb Statements
  Statements behave in the usual way, like in Pascal for instance.

        if b then e1 else e2 -> | e1    when b -> true
                                | e2    when b -> false

        while b do expr         repeat expr while b is true

        for i = l to h do expr  repeat expr for every value of i
                                between l and h (upwards from l)
                                l and h are integers
        for i = h downto l do expr      (downwards from h)

IIIc Pattern matching
  Pattern matching can occur in the following three cases
        * function call, when one of the function parameters (in its
          definition) is a pattern.
        * value definition, if the left hand side is a pattern
        * case statement

  Pattern matching tries to check wether a value matches a pattern,
  assigning corresponding values to variables in the pattern (which
  are all considered free).

  If it succeeds, then evaluation continues, with new definitions for
  pattern variables. In a case statement the current branch is chosen,
  and all other branches discarded.

  If it fails, in the two first cases there is an error message
  telling in which function pattern matching failed. In a case
  statement, the current branch is discarded, and pattern matching is
  attempted again with the next pattern. If all patterns fail, you get 
  an error message.

  (There are no warnings at compile time about completeness of pattern 
  matching, but this operational approach is somehow easier to
  understand for small programs.)

IV The standard library

IVa Operators
  Arithmetic operators
        + - * / mod     int -> int -> int
        +. -. *. /. **  float -> float -> float 
        -               int -> int
        -.              float -> float
    These operators (except mod and **) are both defined for integers
    and floats. Operators of the first line are lightly overloaded: if
    one of their arguments is known to be float, they will
    automatically be converted to the second line operator.
  Equality tests
        =  <>  <  > <= >=       'a -> 'a -> bool
        == !=                   'a -> 'a -> bool
    Operators of the first line check recursively values for
    (in)equality.
    Those of the second line only check physical addresses. They are
    faster and always terminate, but while a == b implies a = b, the
    reciprocal is not true.
  Boolean operators
        &  or           bool -> bool -> bool
    They behave sequentially: if the first argument to & is false, or
    the first argument to or is true, then their second argument will
    not be evaluated.
  Bitwise operators
        land lor lxor lsl lsr   int -> int -> int
    They provide bit-level operations. Integers have 31 bits on 32 bit
    machines.
  Concatenation
        @               'a list -> 'a list -> 'a list
        ^               'a array -> 'a array -> 'a array
    As all other functions on arrays, ^ may be used on strings.

IVb Array operations
        length          'a array -> int
        array           int -> 'a -> 'a array
                "array n a" creates an array of size n filled with a.
        copy            'a array -> 'a array
                "copy arr" returns a copy of arr.
                (assigning to the copy will not modify arr)
        sub             'a array -> int -> int -> 'a array
                "sub arr pos len" returns the sub-array of length len
                starting at position pos in arr.
        map_array       ('a -> 'b) -> 'a array -> 'b array
                "map_array f arr" applies f to every element of arr and
                returns the array of results.
        do_array        ('a -> 'b) -> 'a array -> unit
                same as map_array, but discard results

IVc List operations
        map_list        ('a -> 'b) -> 'a list -> 'b list
        do_list         ('a -> 'b) -> 'a list -> unit

IVd Conversions
        array_of_list   'a list -> 'a array
        list_of_array   'a array -> 'a list

        string_of_int   int -> string
        int_of_string   string -> int
        string_of_float float -> string
        float_of_string string -> float

        char            int -> char
        code            char -> int

        float           int -> float
        round trunc     float -> int

        format_float    int -> bool -> float -> string
                "format_float prec exp x" converts x to a string,
                with prec digits after the decimal dot (or a variable
                number if prec is negative), using exponential
                notation if exp is true.

IVe I/O operations
        print_string    string -> unit
        print_int       int -> unit
        print_float     float -> unit
                print their argument on the output.
        newline         unit -> unit
                "newline ()" creates a new line in the output.
        write_file      string -> string list -> unit
                "write_file name lines" creates a new file "name"
                composed of the given lines.

        read_string     unit -> string
        read_int        unit -> int
        read_float      unit -> float
                read a string/int/float from the input.
        read_file       string -> string list
                read the contents of a file as a list of lines.

IVf Mathematical functions
        not             bool -> bool
                boolean not
        maxint minint   int
                1073741823 and -1073741824
        exp log sqrt sin cos tan asin acos atan abs
                        float -> float
                logarithmic and trigonometric functions.
        random_init     int -> unit
        random_int      int -> int
        random_float    float -> float
                pseudo random numbers.

IVg System and miscellanous
    Quit session
        quit            unit -> unit
                "quit ()" exits the interpreter
    Load a program
        use             string -> unit
                "use name" reads file name (extension ".mal" is added) 
                add puts all definitions in the toplevel environment.
        require         string -> unit
                same as "use", but read the file only if it was not
                already done.
    Exceptions
        raise           string -> 'a
                "raise err" halts the program displaying message err.
    Debugging
        trace           string -> unit
                "trace f" traces all calls to (toplevel defined)
                function f.
        tracen          int -> string -> unit
                "tracen n f" traces calls like "trace f", but only
                displays messages when f is applied to more than n
                arguments.
        untrace         string -> unit
        untrace_all     unit -> unit
    OS-related
        system          string -> int
                call an operating system command, returning the exit code.
        time            unit -> float
                returns the process execution time in seconds.

IVh Graphical extensions
    NB) On MacOSX, this extension uses the X11 provide by Apple.
        You should run minimal-graph from xterm or other.

    General
        open_graph      unit -> unit
        close_graph     unit -> unit
                open and close the graphic window.
        clear_graph     unit -> unit
        size_x size_y   unit -> int
                sizes of the graphic window.
    Colors
        set_color       color -> unit
                set the color for all graphical operations.
        rgb             int -> int -> int -> color
                define a color in the 24 bit RBG model.
        black white red green blue yellow cyan magenta          color
                predefined colors
        point_color     int -> int -> color
                get the color of a point (this is actually an int)
    Drawing
        plot            int -> int -> unit
                "plot x y" sets the color of pixel x y to the current
                color, and moves the current point there.
        moveto          int -> int -> unit
                "moveto x y" sets the current point
        current_point   unit -> int * int
                current point coordinates
        set_line_width  int -> unit
                set the line width for all drawing operations.
        lineto          int -> int -> unit
                "lineto x y" draws a line from the current point to x
                y and moves the current point there.
        fill_rect       int -> int -> int -> int -> unit
                "fill_rect x y w h" draws a rectangle of width w and
                height h at x y, and fills it with current color.
        set_font        string -> unit
        set_font_size   int -> unit
                system dependant.
        draw_string     string -> unit
                draw a string at current point using current font.
   Window Manager I/O
        read_point      unit -> int * int
                read coordinates with the mouse (the user must click).
        key_pressed     unit -> bool
                check wether a key is pressed.
        read_key        unit -> char
                read the pressed key.
        sound           int -> int -> unit
                "sound f t" plays the frequency f for t milliseconds.

$Id: Readme.txt,v 1.4 2004/09/24 05:24:50 garrigue Exp $
