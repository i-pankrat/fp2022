open Format

let load_stdlib typ_env interpret_env input =
  match Parser.parse input with
  | Ok ast ->
    (match Inferencer.check_types ~env:typ_env ast with
     | Ok (typ_env, _) ->
       (match Interpret.run ~env:interpret_env ast with
        | Ok (interpret_env, _) -> typ_env, interpret_env
        | Error e ->
          printf "Failed to load stlib: %a%!" Interpret.pp_ierror e;
          typ_env, interpret_env)
     | Error e ->
       printf "Failed to load stlib: %a%!" Inferencer.pp_error e;
       typ_env, interpret_env)
  | Error e ->
    printf "Failed to load stlib: %a%!" Parser.pp_error e;
    typ_env, interpret_env
;;
