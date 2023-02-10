open Base
open Scheme_delim_lib.Parser
open Scheme_delim_lib.Env
open Scheme_delim_lib.Interpreter

let test_data =
  [ (* {|(display `(1 ,(+ 1 1) ,((lambda () 3)) ,(lambda () 4)))|};
  {|(display '(1 '(2 '(3 4))))|}; *) ]
;;

List.map test_data ~f:(fun input ->
  match parse input with
  | Error e -> Format.print_string e
  | Ok ast ->
    (* Format.printf "%a" pp_program ast; *)
    let env = Env.empty (module String) in
    (match Interpreter.eval_program env ast with
     | Ok (res, _) ->
       Format.printf "\nEval res: %a\n" Interpreter.pp_value res;
       Format.print_string "\n\n<------------------------->\n\n"
     | Error e -> Format.print_string e))
