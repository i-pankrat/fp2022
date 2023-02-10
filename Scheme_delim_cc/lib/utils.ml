(** Copyright 2021-2022, ArtemKhel and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

let ( #% ) = Format.sprintf
let return = Result.return
let ( let* ) res f = Result.bind res ~f
