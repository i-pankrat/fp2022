open Base
open Scheme_delim_lib.Interpreter
open Scheme_delim_lib.Parser
open Scheme_delim_lib.Env

let args =
  match Array.to_list Sys.argv with
  | [ _; path ] -> Some path
  | _ -> None
;;

let read_file path =
  let readline file =
    try Some (input_line file) with
    | End_of_file -> None
  in
  let rec readlines file acc =
    match readline file with
    | Some line -> readlines file (acc ^ line)
    | None -> acc
  in
  let file = open_in path in
  Ok (readlines file "")
;;

let run_repl env code =
  match parse code with
  | Ok ast ->
    (match Interpreter.eval_program env ast with
     | Ok (res, env) ->
       print_endline @@ Interpreter.show_value res;
       print_newline ();
       env
     | Error msg ->
       print_endline msg;
       env)
  | Error msg ->
    print_endline msg;
    env
;;

let run code =
  match parse code with
  | Ok ast ->
    let env = Env.empty (module String) in
    (match Interpreter.eval_program env ast with
     | Ok _ -> print_newline ()
     | Error msg -> print_endline msg)
  | Error msg -> print_endline msg
;;

let rec repl env =
  match run_repl env (Stdio.In_channel.input_all Caml.stdin) with
  | env -> repl env
;;

let () =
  match args with
  | None ->
    print_endline "Scheme REPL";
    let env = Env.empty (module String) in
    repl env
  | Some path ->
    (match read_file path with
     | Ok s -> run s
     | Error e -> Format.print_string e)
;;
