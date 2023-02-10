(** Copyright 2022-2023, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

type error =
  | UnknownVariable of string
  | ParsingError of string
  | WrongVariableType of string
  | EmptyStatementBlock of string
  | ExpressionExpected of string
  | ReturnTypeMismatch of string
  | TypeMismatch of string
  | DivisionByZero of string
  | VarWithoutValue of string
  | MemoryEnd of string
  | IndexOutOfRange of string
  | BadCondition of string
  | StatementOutsideOfLoop of string
  | InvalidFuncCall of string
  | Unreachable
