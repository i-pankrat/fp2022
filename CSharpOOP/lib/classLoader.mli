(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val equals_key : string

module ClassLoader : functor (M : ResultMonad.MONADERROR) -> sig
  val is_public : Ast.modifiers list -> bool
  val is_private : Ast.modifiers list -> bool
  val is_protected : Ast.modifiers list -> bool
  val is_abstract : Ast.modifiers list -> bool
  val is_static : Ast.modifiers list -> bool
  val is_const : Ast.modifiers list -> bool
  val is_virtual : Ast.modifiers list -> bool
  val is_override : Ast.modifiers list -> bool
  val is_new : Ast.modifiers list -> bool
  val get_element_option : KeyMap.KeyMap.key -> 'a KeyMap.KeyMap.t -> 'a option

  val update_element :
    'a KeyMap.KeyMap.t -> KeyMap.KeyMap.key -> 'a -> 'a KeyMap.KeyMap.t

  val load :
    Ast.objects list ->
    Ast.table_class KeyMap.KeyMap.t ->
    Ast.table_class KeyMap.KeyMap.t M.t
end
