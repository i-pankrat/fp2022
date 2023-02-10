(** Copyright 2022-2023, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ruby_lib
open Parser

let ruby s =
  match parse s with
  | Ok e ->
    let open Interpret.Eval (Interpret.Result) in
    (match eval e with
     | Ok { ret; _ } -> pp_value Format.std_formatter ret
     | Error e -> print_endline e)
  | Error e -> print_endline e
;;

let () = ruby (Stdio.In_channel.input_all stdin)
