(* $Id: common.ml,v 1.5 2004/09/24 00:51:15 garrigue Exp $ *)

open Misc

type 'a option =
  | None
  | Some of 'a

type constant =
  | Cint of int
  | Cchar of wchar
  | Cfloat of float

let pprint_const ppf = function
  | Cint i -> Fmt.pf ppf "@[<hov 2> Cint@ %d@]" i
  | Cchar c -> Fmt.pf ppf "@[<hov 2> Cchar@ %c@]" @@ char_of_int c
  | Cfloat f -> Fmt.pf ppf "@[<hov 2> Cfloat@ %f@]" f
;;

type dirflag =
  | Upto
  | Downto

and access =
  | Mutable
  | Immutable
  | Forbidden

let pprint_access ppf = function
  | Mutable -> Fmt.pf ppf "Mutable"
  | Immutable -> Fmt.pf ppf "Immutable"
  | Forbidden -> Fmt.pf ppf "Forbidden"
;;

type ident =
  { name : string
  ; index : int
  }

let id_count = ref 0

let new_id name =
  incr id_count;
  { name; index = !id_count }
;;

let same_id id1 id2 = id1.index = id2.index
and compare_id id1 id2 = id1.index - id2.index

module IdMap = Map.Make (struct
  type t = ident

  let compare = compare_id
end)

module StrMap = Map.Make (struct
  type t = string

  let compare = compare
end)
