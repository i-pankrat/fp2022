(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module KeyMap = struct
  include Map.Make (String)

  let pp pp_v ppf map =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) map;
    Format.fprintf ppf "@]]@]"
end
