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

(* Inspect added basic types in [types_map] *)
let%expect_test _ =
  IdMap.iter
    (fun id info ->
      Format.printf
        "@[<hov 2>%a has type info@ %a@]@ "
        pp_ident
        id
        pp_type_info
        info)
    !types_map;
  [%expect
    {|
    { Common.name = "int"; index = 1 } has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "int"; index = 1 }, []));
        ti_kind = Types.Kbasic }
    { Common.name = "char"; index = 2 } has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "char"; index = 2 }, []));
        ti_kind = Types.Kbasic }
    { Common.name = "float"; index = 3 } has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "float"; index = 3 }, []));
        ti_kind = Types.Kbasic }
    { Common.name = "array"; index = 4 } has type info
      { Types.ti_params = [{ Types.link = None; level = 0 }];
        ti_res =
        (Types.Tconstr ({ Common.name = "array"; index = 4 },
           [(Types.Tvar { Types.link = None; level = 0 })]));
        ti_kind = Types.Kbasic }
    { Common.name = "bool"; index = 5 } has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "bool"; index = 5 }, []));
        ti_kind = (Types.Kvariant [("false", []); ("true", [])]) }
    { Common.name = "list"; index = 6 } has type info
      { Types.ti_params = [{ Types.link = None; level = 0 }];
        ti_res =
        (Types.Tconstr ({ Common.name = "list"; index = 6 },
           [(Types.Tvar { Types.link = None; level = 0 })]));
        ti_kind =
        (Types.Kvariant
           [("[]", []);
             ("::",
              [(Types.Tvar { Types.link = None; level = 0 });
                (Types.Tconstr ({ Common.name = "list"; index = 6 },
                   [(Types.Tvar { Types.link = None; level = 0 })]))
                ])
             ])
        }
    { Common.name = "string"; index = 7 } has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "string"; index = 7 }, []));
        ti_kind =
        (Types.Kabbrev
           (Types.Tconstr ({ Common.name = "array"; index = 4 },
              [(Types.Tconstr ({ Common.name = "char"; index = 2 }, []))])))
        }
    { Common.name = "unit"; index = 8 } has type info
      { Types.ti_params = [];
        ti_res = (Types.Tconstr ({ Common.name = "unit"; index = 8 }, []));
        ti_kind = (Types.Kabbrev (Types.Ttuple [])) } |}]
;;

let%test _ =
  let str = Tconstr ({ name = "string"; index = 7 }, []) in
  Unify.expand str = Tconstr (id_array, [ Tconstr (id_char, []) ])
;;

let%test _ =
  let str = Tconstr ({ name = "string"; index = 7 }, []) in
  let var_array = Tconstr (id_array, [ Tvar { link = None; level = 2 } ]) in
  Unify.unify str var_array;
  var_array
  = Tconstr
      (id_array, [ Tvar { link = Some (Tconstr (id_char, [])); level = 2 } ])
;;
