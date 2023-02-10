(** Copyright 2022-2023, Lev Golofastov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamlprintf_lib
open Interpret.Interpret (Interpret.Result)

let () = run (Stdio.In_channel.input_all stdin)
