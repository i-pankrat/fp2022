(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

let print_program program =
  match Parser.parse program with
  | Ok ast ->
    (match Inferencer.check_types ast with
     | Ok _ ->
       (match Interpret.run ast with
        | Ok res -> printf "%a" Pprintvalue.pp_value res
        | Error e -> printf "%a" Interpret.pp_ierror e)
     | Error e -> printf "%a" Inferencer.pp_error e)
  | Error e -> printf "%a" Parser.pp_error e
;;

let%expect_test _ =
  let e = "let x = (1, 2, 3)\n" in
  print_program e;
  [%expect {| (1, 2, 3) |}]
;;
