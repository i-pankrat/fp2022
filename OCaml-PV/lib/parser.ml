(** Copyright 2021-2022, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)

open Angstrom
open Ast
open Base

(* Exlanation of parser combinators

  <|> - choise operator

  АБСТРАГИРОВАННОЕ ПОНЯТИЕ ПОСЛЕДОВАТЕЛЬНОГО ИСПОЛНЕНИЯ
  let* same as p >>= f - bind. Применяем один парсер p, 
  если он отработает, то запускаем функцию f, которая в зависимости от результата
  возвращает парсер, который мы применяем к результату p 
  если нет, то возвращаем ошибку, не применяя функцию f. В зависимости от 
  того, что мы напарсили в p, мы принимаем решение, как мы будем парсить дальше 

  SPEAK ABOUT BIND: это всегда работа в некотормо контексте


  p >>| f, если парсер на исходной строке отработает успешно, то
  вернёт результат f(x), то есть применит функцию к результату парсера

  АБСТРАГИРОВАННОЕ ПОНЯТИЕ ПРИМЕНЕНИЯ ФУНКЦИИ В КОНТЕКСТЕ
  Любая монада является апликативом
  f <*> p - apply - тоже самое, что  f >>= fun f -> p >>| f, то есть ??
  Приминение функции f:(a -> b)t к значению типа альфа, и получение значения
  резульатта типа b, когда всё это погружено в контекст указанонго типа t.

  f <$> p is equivalent to p >>| f, то есть это просто запись на оборот,
  тут мы применяем парсер p к строке, а затем, если парсер отработал успешно,
  то применяем функцию f, к результату парсера

  p1 *> p2, отбрасывает то, что напарсил p1, и передаёт нераспаршенную часть в p2
  p1 <* p2,  тоже самое, только в обратную сторону: запускаем p1, отбрасываем результат p2

  lift2 f p1 p2 = f <$> p1 <*> p2 - применяем один из парсеров p1, если p1
  не срабатывает, то p2, а затем к результату p1 или p2 применяем функцию f

  ^ - string concatenation

*)

(*  actions to be done to test functions in REPL (utop) 
    if there is no Ast module, just
    1. dune utop . 
    2. #require "OCaml-PV.Lib";; 
    3. #use "parser.ml";; *)

let is_empty = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_valid_symbol = function
  | '_' -> true
  | c -> is_letter c
;;

let is_kw = function
  | "let" | "match" | "if" | "else" | "than" | "fun" | "with" -> true
  | _ -> false
;;

let empty = take_while is_empty
let empty1 = take_while1 is_empty
let token s = empty *> s
let between l r p = l *> p <* r

(* constructors *)

(* Try to parse constants*)

let pCInt =
  token @@ take_while is_digit
  >>= (fun res -> return @@ Int.of_string res)
  >>= fun res -> return @@ CInt res
;;

let pCBool =
  token @@ string "true"
  <|> token @@ string "false"
  >>= (fun res -> return @@ Bool.of_string res)
  >>= fun res -> return @@ CBool res
;;

(* example "str" -> CString str*)
let pCString =
  token @@ (string "\"" *> take_while (fun x -> x != '"'))
  <* token @@ string "\""
  >>= fun res -> return @@ CString res
;;

let pNil =
  token @@ (string "[" *> skip_while (fun x -> x != ']'))
  <* token @@ string "]"
  >>= fun () -> return @@ CNil
;;

let pUnit =
  token @@ (string "(" *> skip_while (fun x -> x != ')'))
  <* token @@ string ")"
  >>= fun () -> return @@ CUnit
;;

let pConst = pCBool <|> pCString <|> pNil <|> pUnit
