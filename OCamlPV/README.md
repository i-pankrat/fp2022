### An implementaion of OCaml mini-language with poly variants

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Ilya Pankratov, i.pankratov.main@gmail.com

Features done:

- AST
- Parser
- Inferencer 
- Interpret
- Add parsing of type declaration
- Add polymorphic variants to parser, inferencer and interpter
- Create unit and crum tests
- Implement REPL

Features not implemented:

- Type declarations are not integrated at inferencer and interpret

Bugs and problems:

- Parsing an application only works if it is enclosed in brackets (Big trouble)
- Other minor parsing troubles

Overview:

Interpret and inferencer are working quite well. However, I am having difficulties with fixing bugs in the parser. Still trying to fix them...