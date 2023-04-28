(* $Id: predef.ml,v 1.6 2004/09/24 00:51:16 garrigue Exp $ *)

open Common
open Types
open Define

(* types *)

let id_int = new_id "int"
and id_char = new_id "char"
and id_float = new_id "float"
and id_array = new_id "array"
and id_bool = new_id "bool"
and id_list = new_id "list"

let basic_types =
  let tv = Unify.newvar () in
  [ "int", make_ti id_int [] Kbasic
  ; "char", make_ti id_char [] Kbasic
  ; "float", make_ti id_float [] Kbasic
  ; "array", make_ti id_array [ tv ] Kbasic
  ; "bool", make_ti id_bool [] (Kvariant [ "false", []; "true", [] ])
  ; ( "list"
    , make_ti
        id_list
        [ tv ]
        (Kvariant
           [ "[]", []; "::", [ Tvar tv; Tconstr (id_list, [ Tvar tv ]) ] ]) )
  ; "unit", make_ti (new_id "unit") [] (Kabbrev (Ttuple []))
  ; ( "string"
    , make_ti
        (new_id "string")
        []
        (Kabbrev (Tconstr (id_array, [ Tconstr (id_char, []) ]))) )
  ]
;;

List.iter (fun (name, info) -> add_type name info) basic_types

(* Inspect added basic types in [types] *)
let%expect_test _ =
  Hashtbl.iter
    (fun name info ->
      Format.printf
        "@[<hov 2>%s has type info@ %a@]@ "
        name
        Types.pp_type_info
        info)
    Define.types;
  [%expect
    {|
    char has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "char"; index = 2 }, []));
        ti_kind = Types.Kbasic }
    array has type info
      { Types.ti_params = [{ Types.link = None; level = 3 }];
        ti_res =
        (Types.Tconstr ({ Common.name = "array"; index = 4 },
           [(Types.Tvar { Types.link = None; level = 3 })]));
        ti_kind = Types.Kbasic }
    bool has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "bool"; index = 5 }, []));
        ti_kind = (Types.Kvariant [("false", []); ("true", [])]) }
    string has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "string"; index = 7 }, []));
        ti_kind =
        (Types.Kabbrev
           (Types.Tconstr ({ Common.name = "array"; index = 4 },
              [(Types.Tconstr ({ Common.name = "char"; index = 2 }, []))])))
        }
    unit has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "unit"; index = 8 }, []));
        ti_kind = (Types.Kabbrev (Types.Ttuple [])) }
    int has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "int"; index = 1 }, []));
        ti_kind = Types.Kbasic }
    float has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "float"; index = 3 }, []));
        ti_kind = Types.Kbasic }
    list has type info
      { Types.ti_params = [{ Types.link = None; level = 3 }];
        ti_res =
        (Types.Tconstr ({ Common.name = "list"; index = 6 },
           [(Types.Tvar { Types.link = None; level = 3 })]));
        ti_kind =
        (Types.Kvariant
           [("[]", []);
             ("::",
              [(Types.Tvar { Types.link = None; level = 3 });
                (Types.Tconstr ({ Common.name = "list"; index = 6 },
                   [(Types.Tvar { Types.link = None; level = 3 })]))
                ])
             ])
        } |}]
;;
