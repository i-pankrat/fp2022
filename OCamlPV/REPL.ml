(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_pv_lib
open Format

let print_prompt () = printf "\n# %!"

let print_hello () =
  printf
    "\n\
     Hello! It's an OCaml interpreter. Stdlib is already loaded!\n\
     You may use the following functions: \n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \t%s\n\
     \tPrint \"exit\" to leave interpret"
    "max: ('a -> ('a -> 'a))"
    "min: ('a -> ('a -> 'a))"
    "list_rev: ('a list -> 'a list)"
    "list_map: (('a -> 'b) -> ('a list -> 'b list))"
    "list_fold: ('a list -> ('b -> (('b -> ('a -> 'b)) -> 'b)))"
    "list_append: ('a list -> ('a list -> 'a list))"
    "list_concat: ('a list list -> 'a list)"
    "list_filter: ('a list -> (('a -> bool) -> 'a list))"
    "list_nth_opt: ('a list -> (int -> [> `None | `Some of 'a ]))"
    "list_find_opt: (('a -> bool) -> ('a list -> [> `None | `Some of 'a ]))"
    "pair_fst: (('a * 'b) -> 'a)"
    "pair_snd: (('a * 'b) -> 'b)"
    "list_assoc_opt: ('a -> (('a * 'b) list -> [> `None | `Some of 'b ]))"
    "list_split: (('a * 'b) list -> ('a list * 'b list))"
;;

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

let () =
  print_hello ();
  let tenc, ienv = Utils.load_stdlib Inferencer.empty Interpret.empty Stdlib.std_lib in
  repl tenc ienv
;;
