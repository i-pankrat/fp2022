(** Copyright 2021-2022, Mikhail Vyrodov and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

open C_lib

let () =
  let s = Stdio.In_channel.input_all Caml.stdin in
  match C_lib.Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n%!" Ast.pp_function_list ast
  | Error _ -> Format.printf "Parsing error"
