### An implementaion of OCaml mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Ilya Pankratov, i.pankratov.main@gmail.com

Features done (append only):

- AST
- Parser
- ...

Features in progress (and TODOs):

- Fix parser after changing AST
- Add paring of function with multiple arguments: let sum a b = a + b not only as let sum a = fun b -> a + b
- Add tuple parser in case without braces: a, b -> PTuple[PVar a; PVar b]
- Fix case with minus parser, if it's possible: a - 1 -> (EApply ((EVar "x"), (EConst (CInt -1)))
- Type infernece
- Interpretator
- REPL
