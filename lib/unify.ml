(* $Id: unify.ml,v 1.6 2004/09/24 00:51:17 garrigue Exp $ *)

open Common
open Types
open Define

exception Unify

let current_level = ref 0
and global_level = ref 1
and generic_level = -1

let begin_def () = incr current_level
and end_def () = decr current_level

let reset_global_level () = global_level := !current_level + 1
and new_global_var () = { link = None; level = !global_level }

let newvar () = { link = None; level = !current_level }

let map_type f = function
  | Tvar _ as ty -> ty
  | Tarrow (ty1, ty2) -> Tarrow (f ty1, f ty2)
  | Ttuple tyl -> Ttuple (List.map f tyl)
  | Tconstr (id, tyl) -> Tconstr (id, List.map f tyl)
;;

(* Apply [f] to [type_def] recursively, except for the toplevel [Tvar] *)
let do_type f = function
  | Tvar _ -> ()
  | Tarrow (ty1, ty2) ->
    f ty1;
    f ty2
  | Ttuple tyl -> List.iter f tyl
  | Tconstr (_, tyl) -> List.iter f tyl
;;

(* Do nothing for var *)
let%expect_test _ =
  do_type (pp_type_expr Format.std_formatter) (Tvar { link = None; level = 0 });
  [%expect {| |}]
;;

(* Do nothing for empty argument constructor *)
let%expect_test _ =
  do_type
    (pp_type_expr Format.std_formatter)
    (Tconstr ({ name = "_"; index = 0 }, []));
  [%expect {| |}]
;;

(* Do something for the argument list *)
let%expect_test _ =
  do_type
    (pp_type_expr Format.std_formatter)
    (Tconstr ({ name = "_"; index = 0 }, [ Tvar { link = None; level = 0 } ]));
  [%expect {| (Types.Tvar { Types.link = None; level = 0 }) |}]
;;

let rec repr = function
  | Tvar ({ link = Some ty; _ } as tv) ->
    let ty' = repr ty in
    if ty' != ty then tv.link <- Some ty';
    ty'
  | ty -> ty
;;

let%test _ =
  let test_var = Tvar { link = None; level = 0 } in
  repr test_var = test_var
;;

let%test _ =
  let test_var = Tvar { link = None; level = 0 } in
  let test_linked_var = Tvar { link = Some test_var; level = 0 } in
  repr test_linked_var = test_var
;;

let%test _ =
  let test_var = Tvar { link = None; level = 0 } in
  let test_linked_var = Tvar { link = Some test_var; level = 0 } in
  let test_linked_linked_var =
    Tvar { link = Some test_linked_var; level = 0 }
  in
  repr test_linked_linked_var = test_var
  && test_linked_linked_var = test_linked_var
;;

let rec generalize ty =
  match repr ty with
  | Tvar tv -> if tv.level > !current_level then tv.level <- generic_level
  | ty -> do_type generalize ty
;;

let%test _ =
  let old_level = !current_level in
  current_level := 5;
  let top_var = Tvar { link = None; level = 4 } in
  generalize top_var;
  current_level := old_level;
  top_var = Tvar { link = None; level = 4 }
;;

let%test _ =
  let old_level = !current_level in
  current_level := 5;
  let top_var = Tvar { link = None; level = 6 } in
  generalize top_var;
  current_level := old_level;
  top_var = Tvar { link = None; level = -1 }
;;

let%test _ =
  let old_level = !current_level in
  current_level := 3;
  let constr =
    Tconstr ({ name = "_"; index = 4 }, [ Tvar { link = None; level = 2 } ])
  in
  let constr2 =
    Tconstr ({ name = "_"; index = 2 }, [ Tvar { link = None; level = 4 } ])
  in
  generalize constr;
  generalize constr2;
  current_level := old_level;
  constr
  = Tconstr ({ name = "_"; index = 4 }, [ Tvar { link = None; level = 2 } ])
  && constr2
     = Tconstr ({ name = "_"; index = 2 }, [ Tvar { link = None; level = -1 } ])
;;

let rec make_nongen ty =
  match repr ty with
  | Tvar tv -> if tv.level > !current_level then tv.level <- !current_level
  | ty -> do_type make_nongen ty
;;

let%test _ =
  let old_level = !current_level in
  current_level := 5;
  let top_var = Tvar { link = None; level = 4 } in
  make_nongen top_var;
  current_level := old_level;
  top_var = Tvar { link = None; level = 4 }
;;

let%test _ =
  let old_level = !current_level in
  current_level := 5;
  let top_var = Tvar { link = None; level = 6 } in
  make_nongen top_var;
  current_level := old_level;
  top_var = Tvar { link = None; level = 5 }
;;

let%test _ =
  let old_level = !current_level in
  current_level := 3;
  let constr =
    Tconstr ({ name = "_"; index = 4 }, [ Tvar { link = None; level = 2 } ])
  in
  let constr2 =
    Tconstr ({ name = "_"; index = 2 }, [ Tvar { link = None; level = 4 } ])
  in
  make_nongen constr;
  make_nongen constr2;
  current_level := old_level;
  constr
  = Tconstr ({ name = "_"; index = 4 }, [ Tvar { link = None; level = 2 } ])
  && constr2
     = Tconstr ({ name = "_"; index = 2 }, [ Tvar { link = None; level = 3 } ])
;;

(* Substitute & update link (side effect) *)
let rec subst s ty =
  match repr ty with
  | Tvar tv as ty ->
    (try List.assq tv s with
     | Not_found -> ty)
  | ty -> map_type (subst s) ty
;;

let%test _ =
  let var1 = { link = None; level = 0 } in
  let test_var = Tvar var1 in
  let var2 = { link = Some test_var; level = 0 } in
  let test_linked_var = Tvar var2 in
  let var3 = { link = Some test_linked_var; level = 0 } in
  let test_linked_linked_var = Tvar var3 in
  let s =
    [ var1, test_linked_linked_var; var2, test_var; var3, test_linked_var ]
  in
  let tvars = [ test_var; test_linked_var; test_linked_linked_var ] in
  (not
     (List.for_all2
        (fun tvar typ -> subst s tvar = typ)
        tvars
        (List.init 3 (fun _ -> test_linked_var))))
  (* First try fails *)
  && (List.iter (fun tvar -> ignore (subst s tvar)) tvars;
      (* After side effect, second try passes *)
      List.for_all2
        (fun tvar typ -> subst s tvar = typ)
        tvars
        (List.init 3 (fun _ -> test_linked_var)))
  && test_linked_linked_var = test_linked_var
  && test_linked_var
     = Tvar { link = Some (Tvar { link = None; level = 0 }); level = 0 }
;;

let rec occur tv ty =
  match repr ty with
  | Tvar tv' ->
    if tv == tv' then raise Unify;
    if tv'.level > tv.level then tv'.level <- tv.level
  | ty -> do_type (occur tv) ty
;;

let%test _ =
  let var0 = { link = None; level = 2 } in
  let var1 = { link = None; level = 3 } in
  let test_var = Tvar var1 in
  let var2 = { link = Some test_var; level = 2 } in
  let test_linked_var = Tvar var2 in
  let var3 = { link = Some test_linked_var; level = 1 } in
  let test_linked_linked_var = Tvar var3 in
  let tvars = [ test_var; test_linked_var; test_linked_linked_var ] in
  List.iter (occur var0) tvars;
  (try
     occur var1 test_var;
     false
   with
   | Unify -> true
   | _ -> false)
  && test_var = Tvar { link = None; level = 2 }
  && test_linked_var = Tvar { link = Some test_var; level = 2 }
  && test_linked_linked_var = Tvar { link = Some test_var; level = 1 }
;;

let expand = function
  | Tconstr (id, tyl) ->
    let info = IdMap.find id !types_map in
    (match info.ti_kind with
     | Kabbrev ty -> subst (List.combine info.ti_params tyl) ty
     | _ -> raise Not_found)
  | _ -> raise Not_found
;;

(* See [Predef] for [expand]'s unit test *)

let rec unify ty1 ty2 =
  let ty1 = repr ty1
  and ty2 = repr ty2 in
  if ty1 == ty2
  then ()
  else (
    match ty1, ty2 with
    | Tvar tv1, Tvar tv2 ->
      if tv1.level > tv2.level
      then tv1.level <- tv2.level
      else tv2.level <- tv1.level;
      tv1.link <- Some ty2
    | Tvar tv, _ ->
      occur tv ty2;
      tv.link <- Some ty2
    | _, Tvar tv ->
      occur tv ty1;
      tv.link <- Some ty1
    | Tarrow (t1, t2), Tarrow (u1, u2) ->
      unify t1 u1;
      unify t2 u2
    | Ttuple tl1, Ttuple tl2 ->
      if List.length tl1 <> List.length tl2 then raise Unify;
      List.iter2 unify tl1 tl2
    | Tconstr (id1, tl1), Tconstr (id2, tl2) when same_id id1 id2 ->
      List.iter2 unify tl1 tl2
    | _ ->
      (try unify (expand ty1) ty2 with
       | Not_found ->
         (try unify ty1 (expand ty2) with
          | Not_found -> raise Unify)))
;;

let%test _ =
  let tvar1, tvar2 =
    Tvar { link = None; level = 0 }, Tvar { link = None; level = 0 }
  in
  unify tvar1 tvar2;
  tvar1 = Tvar { link = Some tvar2; level = 0 }
  && tvar2 = Tvar { link = None; level = 0 }
;;

let%test _ =
  let tvar1, tconstr =
    Tvar { link = None; level = 0 }, Tconstr ({ name = "test"; index = 0 }, [])
  in
  unify tvar1 tconstr;
  tvar1 = Tvar { link = Some tconstr; level = 0 }
  && tconstr = Tconstr ({ name = "test"; index = 0 }, [])
;;

let%test _ =
  let tvar1, tconstr =
    Tvar { link = None; level = 0 }, Tconstr ({ name = "test"; index = 0 }, [])
  in
  unify tconstr tvar1;
  tvar1 = Tvar { link = Some tconstr; level = 0 }
  && tconstr = Tconstr ({ name = "test"; index = 0 }, [])
;;

let%test _ =
  let tvar1, tvar2 =
    Tvar { link = None; level = 0 }, Tvar { link = None; level = 2 }
  in
  unify tvar1 tvar2;
  tvar1 = Tvar { link = Some tvar2; level = 0 }
  && tvar2 = Tvar { link = None; level = 0 }
;;

let filter_arrow ty =
  match repr ty with
  | Tarrow (ty1, ty2) -> ty1, ty2
  | ty ->
    let ty1 = Tvar (newvar ())
    and ty2 = Tvar (newvar ()) in
    unify ty (Tarrow (ty1, ty2));
    ty1, ty2
;;
