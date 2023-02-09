(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_pv_lib
open Format

let print_prompt () = printf "\n# %!"

let print_exit () =
  printf "\tThank you for using my OCaml interpret! Hope you enjoy it! :)\n%!"
;;

let run_input typ_env interpret_env input =
  match Parser.parse input with
  | Ok ast ->
    (match Inferencer.check_types ~env:typ_env ast with
     | Ok (typ_env, typ) ->
       (match Interpret.run ~env:interpret_env ast with
        | Ok (interpret_env, res) ->
          printf "%a%!" Printer.pp_result (res, typ);
          typ_env, interpret_env
        | Error e ->
          printf "%a%!" Interpret.pp_ierror e;
          typ_env, interpret_env)
     | Error e ->
       printf "%a%!" Inferencer.pp_error e;
       typ_env, interpret_env)
  | Error e ->
    printf "%a%!" Parser.pp_error e;
    typ_env, interpret_env
;;

let rec repl tenv ienv =
  print_prompt ();
  let input = read_line () in
  if Base.String.strip input = "exit"
  then print_exit ()
  else (
    let tenv, env = run_input tenv ienv input in
    repl tenv env)
;;

let () = repl Inferencer.empty Interpret.empty