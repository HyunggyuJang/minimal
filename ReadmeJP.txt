        Minimal 利用者マニュアル (V1.0)

このマニュアルは三つの部分に分かれている。

  1. 入力ループの使い方
        Minimalの基本的な使用法

  2. 値、実行と制御構造
        言語で扱う概念の細かい定義

  3. 組込関数
        提供されている関数のリスト
        岩波書店の「コンピュータサイエンス入門」に比べて、いくつか関数
        が追加されている。
          ビット演算子   land lor lxor lsl lsr
          入出力         read_file write_file
          変換           int_of_string float_of_string round format_float
          システム他     require system time
          グラフィックス point_color   

サンプルプログラムもいくつか同封されている。
    prelude.mal
       共通の便利な関数。他のサンプルからよく`require "prelude";'とい
      う形で利用されている
    variable.mal, stack.mal, queue.mal, btree.mal
       よくあるデータ構造の実装例
    poly.mal, matrix.mal  数学的な構造
    int32.mal             32ビット整数の処理
    music.mal             小さなピアノ
    graphics.mal          関数グラフを描くためのツール
    turtle.mal            タートルを動かして絵を描きましょう


1. 入力ループの使い方

  Minimal は他の多くのプログラミング言語と違い、プログラムをコンパイル
  して、実行するのではない。入力ループを使い、Minimalと対話しながら、
  値や関数を定義したり、式を計算・実行したりするのだ。プログラムを単位
  とするよりも、このやりかたでは関数やアルゴリズムの動作確認が容易にで
  きるためだ。

a) 立ち上げと終了
  この対話的入力ループに入るために、まずDOS窓やUNIXからMinimalを立ち上
  げる。(Minimalをクリックしてもいい)

        C:\MINIMAL> minimal
        >       MiniMAL version 1.0 (SJIS) on Objective Caml
        
        # 

  最後の # はプロンプトと言い、入力待ちを表している。

  入力ループから出たいとき、quit(); と入力して下さい。

        # quit();
        C:\MINIMAL>

b) 入力
  入力ループで何かを入力する。例えば "1+2;<CR>"

        # 1+2;
        - : int = 3

  ";" は Minimal に入力が終わったことを知らせる。それがないと、<CR>が
  無視され、改行しても入力が続く。

  "- : int = 3" は Minimal の答えだ。
  "-" は入力された式がそのまま実行されたことを表す。
  "int" は入力された式が整数型だったことを表す。ユーザが入力する必要も
  なく、Minimalが自動的にそれを計算してくれる。
  "3" は実行の結果だ。

c) 値と関数の定義
  値を定義すると答えがちょっと違う。

        # val x = 2*2;
        val x : int = 4

  x は定義された値の名前で、int はその型で、そして 4 は x の新しい値で
  ある。その後 x を式の中に書くと、その値が 4 になる。

        # x+1;
        - : int = 5

  関数の定義は同様な表示になる。

        # fun f x = x+1;
        val f : int -> int = <fun>

  Minimal は関数型言語なので、関数は普通の値である。だから、値の定義と
  同様に答は "val" で始まる。型は整数から整数への関数なので、"int ->
  int"。関数の値は表示できないので、値の所が "<fun>" になっている。

d) 変数の定義
  Minimal には値の定義とちょっと違う変数の定義もある。

        # var x = 1;
        var x : int = 1

  変数は値と使い方が違ったりするので、答は "val" ではなく "var" になっ
  た。値と同様に、変数はその後の式の中に使えるが、値と違い、その中身が
  変えられる。

        # x;
        - : int = 1
        # x <- 5;
        - : unit = ()
        # x;
        - : int = 5

  "x <- 5" は x を定義するのではなく、既に定義された整数の変数 x の中
  身を変える。
  その答の "- : unit = ()" は特に意味がない。unit 型にはただ一つの値 
  "()" しか含まれず、式に結果がないときはそれを返す。

  定義されていない変数に値を入れようとしたり、変数の型と違う型の値を入
  れようとしたりすると、エラーになる。

        # y <- 5;
        > Line 1, char 0-5 : unbound identifier y.
        # x <- "hello";
        > Line 1, char 5-11 : 
          Expression has type string where int was expected.

  "unbound identifier" は y と言う名前の変数が存在しないという意味だ。
  二つ目のエラーは変数 x の型が int (整数)なのに、式 "hello" の型が 
  string (文字列)だったので、新しい値を入れる事ができなかったという意
  味だ。

  どちらもよくある間違いで、特に"Expression has type ... where ... was
  expected." という形のエラーは多くのプログラムの間違いを指摘してくれ
  る。それを正しく理解することが重要だ。

e) 型の定義
  組込の型を元に、新しい型を定義することもできる。その説明は次節まで。

f) ファイルからプログラムを読み込む
  最後に、入力ループは定義を入力する唯一の方法ではない。ファイルからそ
  れらを読み込むこともできる。そのとき、入力ループで入力されたときと全
  く同じ働きをする。

  ファイル test.mal の中身は次の通りだとする。

        fun double x = x * 2;           (* double は引数の二倍を計算する *)
        val y = 10;                     (* y を適当な値に *)
        y + double y;                   (* これで三倍だ！ *)

  ファイルの中で (* と *) で囲まれた部分は註釈で、無視される。

  test.mal を読み込む。

        # use"test";
        val double : int -> int = <fun>
        val y : int = 10
        - : int = 30
        - : unit = ()

  最初の三つの答は、読み込んだ入力によるものだ。最後の "- : unit = ()"
  は use 自体の結果だ。use が正常に終了したことを表す。

2. 値、実行と制御構造

a) 基本データ型
  Minimal では様々なデータがそのまま扱える。定数なので、実行の結果はそ
  の値自体になる。

  * 整数(int) 565 -6327 など。
    曖昧なとき負数は(-23)の様に括弧を付けなければならない。

  * 実数(float) 1. -5.3 2e4 など。
    整数と区別するために必ず "." や "e" がなければならない。

  * 文字(char) 'a' '\n' '\027' など。
    最後の形はASCIIコードを三桁の十進数で表す。

  * 文字列(string) "hello" "April 26, 1971" など。
    文字と同様、文字コードも使える。内部的には配列なので、配列に関する
    関数は文字列にも使える。
    注 Minimalは複数の文字コードをサポートして入る。環境変数
    MINIMAL_LANG、LC_ALL、LANGまたはLC_CTYPEの語尾を見て、SJIS、EUC、
    UTF8またはNOCONVに設定される。どの変数も設定されていない場合はSJIS
    になる。

  * 真偽値(bool) true(真) と false(偽) のみ。

  * 単位(unit) 意味のない型。何も返したくないときに使う。() のみ。

b) 定数式
  定数だけでできた式。計算は要らない。

  上記の基本データ型を元に、その組合せなどで値が作れる。
  組込の組合せ方法は三つある。

  * 組 (値1 , ... , 値n) と書く。値の型はばらばらでも構わない。
    その型は要素の型を全て反映し、型1 * ... * 型n になる。
        # (1, 1.0, "one");
        - : int * float * string = (1, 1.0, "one")

  * 配列(array) [| 値1 , ... , 値n |] と書く。
    型は皆同じでなければならない。要素の型が t だったら型は t array に
    なる。
        # [|1, 2, 3|];
        - : int array = [|1, 2, 3|]
    配列の特別ば場合として、string は char array に当る。

  * リスト(list) [値1 , ... , 値n] と書く。
    配列と同様、型は皆同じである。リストの型は t list になる。
        # [1, 2, 3];
        - : int list = [1, 2, 3]
    ただし、配列と違って、要素とリストから、その要素を先頭に加えたリス
    トは計算なしに作れる(並列の結合は計算が要る)。
        # 1 :: [2,3];
        - : int list = [1, 2, 3]
    もっと正しく言うと、[...]を使った書き方は便宜のためにあるだけで、
    リストは全て :: (cons構成子) と [] (空リスト) から作れる。
        # 1 :: 2 :: 3 :: [];
        - : int list = [1, 2, 3]

c) 計算式
  関数や演算子でできた式。定数式も含む。

  例
        2 * (3 + 5)
        4. +. sin 1.5
        cos (1.3 +. 4.5)
        sub array 3 5

  優先順位で関数が最も高いので、式の中で括弧を付けなくてもいい。
  二引数以上の関数もただ並べて書くだけでいい。
        
  演算子とその型は下記のもの
  * 数値演算子
        + - * / mod     int -> int -> int
        +. -. *. /. **  float -> float -> float 
        -               int -> int
        -.              float -> float
    数値演算子は整数用と実数用が別々に定義されている。ただし、引数の型
    が実数だと入力時に分かれば、整数用のものは自動的に実数用に変換され
    る。
  * 比較演算子
        =  <>  <  > <= >=       'a -> 'a -> bool
        == !=                   'a -> 'a -> bool
    型の 'a はどの値でも使えるという意味だ。同じ型の値ならどの値でも比
    較できる。二行目は値のメモリ上の物理的な位置を比較する。
  * 真偽演算子
        &  or           bool -> bool -> bool
    「式1 & 式2」は式1の結果が偽だったら式2を実行しない。
    「式1 or 式2」は式1の結果が真だったら式2を実行しない。
  * ビット演算子
        land lor lxor lsl lsr   int -> int -> int
    ビットごとの演算を提供している。32ビットマシンではintのビット数は
    32である。
  * 結合演算子
        @               'a list -> 'a list -> 'a list
        ^               'a array -> 'a array -> 'a array
    ^ は文字列にも使える。例えば、"hello" ^ " world" は "hello world" 
    になる。
  * 配列演算子
        配列.[添字]             'a array -> int -> 'a
        配列.[添字] <- 値       'a array -> int -> 'a -> unit
    「a.[i]」で配列 a の i 番目(0から)の要素を取り出す。
    「a.[i] <- 式」で配列 a の i 番目の要素を式の結果に書換える。

d) 定義
  既に定義のやりかたを説明したが、もっと形式的に説明すると、こういう形
  の定義ができる。

        val 定数名 = 計算式
        fun 関数名 引数1 引数2 ... 引数n = 計算式
        var 変数名 = 計算式
  
  val と var の場合、式の結果の型がそのまま値や変数の型になるが、fun 
  だと関数には引数の型を含んだ関数型が付けられる。
  上記の定義で引数の型が型1 … 型nで結果の型が型0だとすると、関数の型
  はこうなる

        型1 -> 型2 -> ... -> 型n -> 型0

  数学ではこういうような、矢印が一杯入った型はあまり使わないが、要する
  に (型1 × 型2 × … × 型n) → 型0 と同じようなものだと思えばいい。

  val、fun、var とも、同時に複数の定義ができる。そのとき、and を使う。

        # val x = 3 and y = 2*2;
        val x : int = 3
        val y : int = 4

  ";"を入力せずに複数の定義を同時に入力することもできる。意味は区切ら
  れたものと全く同じである(前に出ている定義が使える)が、一気に計算が行
  われる。

        # val x = 3
          val y = x+1;
        val x : int = 3
        val y : int = 4

e) 制御
  * 選択
        if 計算式1 then 計算式2 else 計算式3
        if 計算式1 then 計算式2
    計算式1 を実行し、その結果が true だったら計算式2を実行し、その結
    果を返す。そうでなければ計算式3を実行し、その結果を返す。計算式3が
    ない場合、計算式2が()を返さなければならない。

  * 条件付きループ
        while 計算式1 do 計算式2
    計算式1がtrueを返す限り、計算式2を実行する。計算式2の返す値と関係
    なく、終わると()を返す。

  * 添字付きループもっと
        for 変数名 = 計算式1 to 計算式2 do 計算式3
        for 変数名 = 計算式1 downto 計算式2 do 計算式3
    計算式1の結果から計算式2の結果までの整数の値を変数に入れ、計算式3
    を実行する。toのときは上り、downtoのときは下る。

f) ブロック構造
  定義や制御構造の中で、右辺に書けるのは一般的なプログラムではなく、計
  算式のみとなっている。

  計算式の中に定義をしたり、連続して複数の計算式を実行したりしたいとき、 
  ブロックを作らなければならない。ブロックは begin で始まり、end で終
  わる。その中に、";" で区切った定義や計算式が書ける。(定義やendの前で
  は";"を省略してもいい)

  ブロックの結果は最後の式の結果になる。定義で終わるとき、()を返す。

        # fun maxabs x y =
          begin
            val x' = if x < 0 then -x else x
            and y' = if y < 0 then -y else y;
            if x' < y' then y' else x'
          end;
        val maxabs : int -> int -> int = <fun>
        # fun minmax array =
          begin
            var max = array.[0];
            var min = array.[0];
            for i = 1 to length array - 1 do
              begin
                if array.[i] > max then max <- array.[i];
                if array.[i] < min then min <- array.[i];
              end;
            (min,max)
          end;
        val minmax : 'a array -> 'a * 'a = <fun>

  maxabs は x と y の絶対値を計算し、大きい方を返す。
  minmax は配列の中の最大値と最小値を計算し、その対を返す。

g) パターン・マッチング
  上記の minmax は対を返すが、対から値を出すのにパターン・マッチングが
  必要だ。

        # val (mn,mx) = minmax [|2,1,4,3|];
        val mn : int = 1
        val mx : int = 4

  そうだ、funやvarと違い、valにそういう便利な機能が付いていたのだ。
  実は、valによる定義はもっと自由だ。

        val パターン = 計算式

  パターンは計算式と同じ書き方をするが、

  * 関数や命令、実行されるようなものは入らない

  * 変数は右辺の計算式によって定義される。もしもその変数は既に定義され
    ていても、元の値が無視される。

        # val (1,x) = (1,3);
        val x : int = 3

  パターンと値が合わないとき、エラーが起こる。

        # val (1,x) = (2,3);
        > Match failure in toplevel input.

  そのエラーを拾って、場合分けができる。

        # case (2,3) of (1,x) => x | (2,x) => x + 2 end;
        - : int = 5

  case文では、値を先に与え、その次にパターンのリストと各場合に応じて実
  行される計算式の列を書く。ブロックと同様、結果は選ばれた場合の最後の
  式の返り値である。

        case 計算式 of
          パターン =>
            計算式 ; ... ; 計算式
        | パターン =>
            計算式 ; ... ; 計算式
        | ...
        end

  ここで見たのは変数、単純な値、そしてその組のパターンだけだが、新しい
  データ型を定義すると、それらをパターンにも使える。

h) 新しいデータ型の定義

  三種類の定義ができる

  1) 略称の定義
    データ構造を新しく定義するのではなく、既に定義されたデータ構造に名
    前を付ける。C言語のtypedefに当る。

        # type complex == float * float;
        type complex defined.

    型推論では略称を自動的に入れないので、手で書かなければならない。

        # fun add_complex ((x,y) : complex) ((x',y') : complex) : complex =
                (x+x',y+y');
        val add_complex : complex -> complex -> complex = <fun>
        # add_complex (1.,2.) (3.,4.);
        - : complex = (4.0, 6.0)

    string も char array の略称だが、この場合だけは自動的に置き換えら
    れる。

  2) 直和型の定義
    様々な種類の内容が入る方である。

    例えば、トランプ。まず色を定義する。
        # type color = Diamond | Heart | Spike | Clove;
        type color defined.
    各構成子は値としても使える。
        # Heart;
        - : color = Heart
    さらにカードを定義する。
        # type card = Normal of color * int | Joker;
        type card defined.
    of の後に構成子の引数を書ける。
        # Normal (Clove,11);
        - : card = Normal (Clove, 11)
        # Joker;
        - : card = Joker

    リストも直和型として定義されている。:: と [] は特別な記号なので、
    新しくリストを定義しようとすると、別の名前を使う。
        # type 'a list = Cons of 'a * 'a list | Nil;
        type list defined.
    上の例と違い、リストは型自身が引数('a)を取る。様々な物のリストがあ
    るからだ。さらに、Cons の二番目の引数もリストである。リストは再帰
    的な型で、Consがいくつかあって、最後にNilで終わるという形になる。
        # Cons(1, Cons(2, Cons (3, Nil)));
        - : int list = Cons (1, Cons (2, Cons (3, Nil)))
    直和型はパターンマッチングにも使える。これはリストの長さを計算する
    ものだ。
        # fun length_list l =
            case l of
              Nil => 0
            | Cons (a,l) => 1 + length_list l
            end;
        val length_list : 'a list -> int = <fun>

    リストでは型引数を 'a と書いたが、もしも複数の引数が必要であれば、
    type ('a,'b,'c) t = ... という形でも書ける。直和型だけでなく、型引
    数は略称型や次の直積型でも使える。

  3) 直積型
    組の要素に名前を付けると便利になる。それを可能にするのは、この直積
    型である。

    複素数の例を直積型で書くとこうなる。
        # type complex = {x : float, y : float};
        type complex defined.
        # val i = {x=0.0,y=1.0};
        val i : complex = {x=0.0, y=1.0}
        # i.y;
        - : float = 1.0
        # fun add_complex a b = {x = a.x + b.x, y = a.y + b.y};
        val add_complex : complex -> complex -> complex = <fun>
        # add_complex i i;
        - : complex = {x=0.0, y=2.0}

    直積型のもう一つの特徴は、ある要素を可変にできることだ。
        # type person = {name : string, mutable age : int};
        type person defined.
        # val me = {name = "Gariko", age = 25};
        val me : person = {name="Gariko", age=25}
        # me.age <- me.age + 1;
        - : unit = ()
        # me;
        - : person = {name="Gariko", age=26}
    こういう可変な直積型の値は、変数と違い、後から定義された関数の中に
    使ったり、渡したりできる。
        # fun birthday pers = pers.age <- pers.age + 1;
        val birthday : person -> unit = <fun>
        # birthday me;
        - : unit = ()
        # me;
        - : person = {name="Gariko", age=27}

3. 組込関数
  組込のデータ型に対して様々な関数が定義されている。

a) 配列
        length          'a array -> int
        array           int -> 'a -> 'a array
  「array n a」は値aで初期化された配列を作る。
        copy            'a array -> 'a array
  「copy arr」は配列arrのコピーを返す。(コピーの変更は元の配列を変えな
  い)
        sub             'a array -> int -> int -> 'a array
  「sub arr pos len」は配列arrのposからの長さlenの部分配列を返す。
        map_array       ('a -> 'b) -> 'a array -> 'b array
  「map_array f arr」は関数fを配列arrの全ての要素に適用し、結果の配列
  を返す。
        do_array        ('a -> 'b) -> 'a array -> unit
  「map_array」と同じだが、結果は返さない。

b) リスト
        map_list        ('a -> 'b) -> 'a list -> 'b list
        do_list         ('a -> 'b) -> 'a list -> unit
  配列と同じ動作をリストに対して行う。

c) 変換
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
                「format_float prec exp x」は x を文字列に変換する。
                prec が正の整数なら、小数点以下は prec に固定される。
                exp が true なら、指数表示が使われる。

d) 入出力
        print_string    string -> unit
        print_int       int -> unit
        print_float     float -> unit
                引数の値を表示する
        newline         unit -> unit
                「newline ()」は出力に改行を強いる。
        write_file      string -> string list -> unit
                「write_file 名前 行」はファイル名前の中に与えられた行
                の文字を書き込む。

        read_string     unit -> string
        read_int        unit -> int
        read_float      unit -> float
                文字列・整数・実数をそれぞれ入力する。
        read_file       string -> string list
                ファイルの中身を行のリストとして読み込む。

e) 数値関数
        not             bool -> bool
                真偽値の否定
        maxint minint   int
                定数：1073741823 と -1073741824
        exp log sqrt sin cos tan asin acos atan abs
                        float -> float
                指数と三角関数
        random_init     int -> unit
        random_int      int -> int
        random_float    float -> float
                擬似乱数の生成

f) システムその他
  Minimalを終了させる
        quit            unit -> unit
  プログラムを読み込む
        use             string -> unit
                「use ファイル名」で「ファイル名.mal」を読み込む。
        require         string -> unit
                まだ読まれていない場合にだけ読み込む。
        cd              string -> unit
                指定されたディレクトリ(またはフォルダ)に移動する。
  例外
        raise           string -> 'a
                「raise メッセージ」はメッセージを表示してプログラムを
                終了させる。
  デバッグ
        trace           string -> unit
                「trace f」はトップレベルで定義された関数fの呼び出しを
                表示する。
        tracen          int -> string -> unit
                「tracen n f」は関数fがn以上の引数に適用される全ての呼
                び出しを表示する。関数fの型が t1 -> .. tn -> t だった
                ら「trace f」は「trace n f」に当る。
        untrace         string -> unit
        untrace_all     unit -> unit
                呼び出しの表示を止める。
  OS関係
        system          string -> int
                OSのコマンドを実行し、終了コードを返す。
        time            unit -> float
                現プロセスの実行時間を秒単位で返します。

g) グラフィックス拡張
  注) MacOSX版ではこの拡張はApple提供のX11を利用しています。
      xtermなどからminimal-graphを起動して下さい。

  一般
        open_graph      unit -> unit
        clear_graph     unit -> unit
        close_graph     unit -> unit
                グラフィックス画面を見せる、消す、閉じる。
        size_x size_y   unit -> int
                グラフィックス画面の幅と高さ。
  色
        set_color       color -> unit
                これから使う色を設定する。
        rgb             int -> int -> int -> color
                24ビットRGBモデルで色を定義する。
        black white red green blue yellow cyan magenta          color
                組込の色。
        point_color     int -> int -> color
                「point_color x y」は座標x,yのピクセルの色を返す。
  描画
        plot            int -> int -> unit
                「plot x y」は座標x,yのピクセルの色を設定の色に変える。
                現在位置もそこに移される。
        moveto          int -> int -> unit
                「moveto x y」は現在位置を設定する。
        current_point   unit -> int * int
                現在位置の座標を返す。
        set_line_width  int -> unit
                線の幅を設定する。
        lineto          int -> int -> unit
                「lineto x y」は現在位置から指定のピクセルまで線を引く。 
                現在位置もそこに移される。
        fill_rect       int -> int -> int -> int -> unit
                「fill_rect x y w h」は設定の色で塗り潰された四角を座
                標x,yから幅wと高さhで描く。
        set_font        string -> unit
        set_font_size   int -> unit
                字体の指定。
        draw_string     string -> unit
                文字列を現在位置に書く。
  入出力
        read_point      unit -> int * int
                マウスを使って、指定された点の座標を読み込む。
        key_pressed     unit -> bool
                キーが押されているかどうかを見る。
        read_key        unit -> char
                押されたキーの文字を読む。
        sound           int -> int -> unit
                「sound f t」は周波数fをt/1000秒間出す。

$Id: ReadmeJP.txt,v 1.5 2004/09/24 06:35:21 garrigue Exp $
