(** Copyright 2021-2022, Ilya Pankratov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Pretty printer goes here *)

open Typedtree

let rec pp_typ ppf =
  let open Format in
  function
  | Ty_var n -> fprintf ppf "'_%d" n
  | Prim s -> pp_print_string ppf s
  | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
  | List t -> fprintf ppf "%a list" pp_typ t
  | Tuple ts ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf " * ")
         (fun ppf ty -> fprintf ppf "%a" pp_typ ty))
      ts
;;
