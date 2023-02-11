### An implementaion of CSharpOOP language in Ocaml

This is a homework for SPBU MM functional programming course

License: LGPL

Author: Alimov Pavel Gennadievich, st076209@student.spbu.ru

Features done:
- AST
- Parser
- Interpreter
- Typechecker
- Test
- PrettyPrinter

In detail:

- Int, string and bool types
- Arithmetic operations
- Conditions (if expression)
- Cycles (while and for)
- Recursion
- Cast types (int to bool, bool to int, children to parent)
- Print function (only Console.WriteLine())
- Class loading
- Classes including inheritance
- Interfaces
- Access modifiers --- public/protected/private
- Inheritance modifiers --- abstract/virtual/override/new

Notes:

- Keywords cannot be class names
- The Main method does not take anything into itself, it is the entry point of the program. The Main method is the only one and may contains only in Program class
- Post- and pref- incrementing equivalents to *+1*