(** Copyright 2022-2023, Denis Porsev and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parsetree
open Typedtree
open Infer
open Interpret
open Interpret (EvalResult)
open Prettyprint

let commands =
  [ "Usage:"
  ; " #help         Prints a list of all available commands"
  ; " #quit         Exit the toplevel loop and terminate this program"
  ; " #use <file>   Read and evaluate source phrases from the given file"
  ]
;;

let repl (typ_env : TypeEnv.t) (val_env : environment) = function
  | Definition (name, exp) ->
    let typ, value = infer exp typ_env, eval exp val_env in
    let t, v =
      match typ, value with
      | Result.Ok t, Result.Ok v ->
        Format.printf "val %s : %a = %a\n" name pp_typ t pp_value v;
        t, v
      | Result.Error type_e, _ ->
        pp_error Format.std_formatter type_e;
        TBase TUndef, VUndef
      | _, Result.Error eval_e ->
        pp_error Format.std_formatter eval_e;
        TBase TUndef, VUndef
    in
    let typ_env, val_env =
      ( TypeEnv.extend typ_env name (Base.Set.empty (module Base.Int), t)
      , IdMap.add name v val_env )
    in
    typ_env, val_env
  | Expression exp ->
    let typ, value = infer exp typ_env, eval exp val_env in
    (match typ, value with
     | Result.Ok t, Result.Ok v -> Format.printf "- : %a = %a\n" pp_typ t pp_value v
     | Result.Error type_e, _ -> pp_error Format.std_formatter type_e
     | _, Result.Error eval_e -> pp_error Format.std_formatter eval_e);
    typ_env, val_env
  | Command c ->
    (match c with
     | Help -> List.iter (Format.printf "%s\n") commands
     | Quit ->
       Format.printf "Quiting...";
       exit 0
     (* TODO: implement #use *)
     | Use file -> Format.printf "#Use file not implemented for now. Your file: %s" file);
    typ_env, val_env
;;

let init_repl is_debug input =
  match Parser.parse_toplevel input with
  | Error e -> Format.printf "\nError: %s\n" e
  | Result.Ok toplevel_input ->
    let rec helper typ_env val_env = function
      | [] -> TypeEnv.empty, IdMap.empty
      | [ h ] -> repl typ_env val_env h
      | h :: tl ->
        let typ_env, val_env = repl typ_env val_env h in
        helper typ_env val_env tl
    in
    let typ_env, val_env = TypeEnv.empty, IdMap.empty in
    if is_debug
    then
      Prettyprint.pp_env
        Format.std_formatter
        (snd (helper typ_env val_env toplevel_input))
    else (
      let _ = helper typ_env val_env toplevel_input in
      ())
;;

let run_repl is_debug =
  (* Format.printf "(mini_repl) | OCaml subset with labeled arguments #\n"; *)
  let rec cycle_repl () =
    let text = read_line () in
    match text with
    | exception End_of_file -> ()
    | exception Failure msg -> Format.printf "Exception: %s" msg
    | _ -> cycle_repl (init_repl is_debug text)
  in
  cycle_repl
;;

let run_single is_debug =
  (* Format.printf "(mini_repl) | OCaml subset with labeled arguments #\n"; *)
  let text = Stdio.In_channel.(input_all stdin) |> Base.String.rstrip in
  init_repl is_debug text
;;
