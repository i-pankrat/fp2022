### An implementaion of "OCaml with labeled and optional arguments" mini-language

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Denis Porsev, den.porsev@gmail.com

--- 

### Implementation features:
#### Features done:

- Parser
- Interpreter
- Pretty-printing for ast types, values and errors
- Simple read-eval-print-loop
- Integration tests
- Type inference

#### Features in progress (and TODOs) (done):

- [x] Type inference with polymorphism is a WIP
- [x] More thorough testing
- [x] Documentation
- [x] Extensions of REPL (new commands, better exception handling)

--- 

### Language features:

#### Supported language features:
- Primitive types: booleans, integers, unit type, lists
- Binary operators: addition, multiplication, subtraction, division, comparisons, logical 'or' and logical 'and'
- Let expressions and definitions
- Functions: basic, anonymous, recursive, closures; partial application is available
- Labeled and optional arguments

You can see these features in use in `demo/demoSingle.t`

#### Possible language extensions:
- Unary operators, pattern matching, option types
