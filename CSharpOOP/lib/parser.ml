(** Copyright 2021-2022, Pavel Alimov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Opal
open List

let apply p s = parse p (LazyStream.of_string s)

module Expression = struct
  let parens parser =
    token "(" >> parser >>= fun result -> token ")" >> return result

  let reserved =
    [
      "true";
      "false";
      "if";
      "else";
      "for";
      "while";
      "public";
      "const";
      "static";
      "int";
      "bool";
      "string";
      "void";
      "char";
      "null";
      "new";
      "this";
      "base";
      "virtual";
      "override";
      "abstract";
      "namespace";
      "using";
      "do";
      "return";
      "continue";
      "break";
      "class";
      "Console";
    ]

  let ident =
    spaces >> letter <~> many alpha_num => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

  let name = ident >>= fun name -> return (Name name)

  let concatenate_name_with_sep sep (Name n1) (Name n2) =
    Name (String.concat "" [ n1; sep; n2 ])

  let%test _ = apply name "Name" = Some (Name "Name")
  let%test _ = apply name "   Name" = Some (Name "Name")
  let%test _ = apply name "     void" = None
  let%test _ = apply name "2two" = None

  let identifier = ident >>= fun identifier -> return (Identifier identifier)

  let%test _ = apply identifier "   Name" = Some (Identifier "Name")
  let%test _ = apply identifier "     true" = None
  let%test _ = apply identifier "2two" = None

  let null = token "null" >> return Null

  let%test _ = apply null "  null" = Some Null

  let const = token "const" >> return Const

  let%test _ = apply const "  const" = Some Const

  let base = token "base" >> return Base

  let%test _ = apply base "      base" = Some Base

  let this = token "this" >> return This

  let%test _ = apply this "         this" = Some This

  let parse_string =
    let string_of_chars chars =
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars;
      Buffer.contents buf
    in
    token "\"" >> many (satisfy (fun x -> x <> '\"')) >>= fun list ->
    token "\"" >> return (Value (VString (string_of_chars list)))

  let%test _ =
    apply parse_string "       \"Hello world!\""
    = Some (Value (VString "Hello world!"))

  let digits = spaces >> many1 digit => implode
  let integer = digits => int_of_string

  let%test _ = apply integer " 1901" = Some 1901

  let int_value = integer >>= fun i -> return (Value (VInt i))

  let%test _ =
    apply int_value "            19012001" = Some (Value (VInt 19012001))

  let%test _ =
    apply int_value "            19012aa1" = Some (Value (VInt 19012))

  let false_value = token "false" >> return (Value (VBool false))
  let true_value = token "true" >> return (Value (VBool true))
  let add_op = token "+" >> return (fun x y -> Add (x, y))
  let sub_op = token "-" >> return (fun x y -> Sub (x, y))
  let mult_op = token "*" >> return (fun x y -> Mult (x, y))
  let div_op = token "/" >> return (fun x y -> Div (x, y))
  let mod_op = token "%" >> return (fun x y -> Mod (x, y))
  let or_op = token "||" >> return (fun x y -> Or (x, y))
  let and_op = token "&&" >> return (fun x y -> And (x, y))
  let less_op = token "<" >> return (fun x y -> Less (x, y))
  let more_op = token ">" >> return (fun x y -> More (x, y))
  let less_or_equal_op = token "<=" >> return (fun x y -> LessOrEqual (x, y))
  let more_or_equal_op = token ">=" >> return (fun x y -> MoreOrEqual (x, y))
  let equal_op = token "==" >> return (fun x y -> Equal (x, y))
  let not_equal_op = token "!=" >> return (fun x y -> NotEqual (x, y))

  let atomic =
    identifier <|> int_value <|> parse_string <|> false_value <|> true_value
    <|> null

  let define_type =
    choice
      [
        token "int" >> return TInt;
        token "string" >> return TString;
        token "void" >> return TVoid;
        token "bool" >> return TBool;
        (ident >>= fun class_name -> return (TRef class_name));
      ]

  let rec expression input = or_expr input
  and or_expr input = (chainl1 and_expr or_op) input
  and and_expr input = (chainl1 comparison_expr and_op) input

  and comparison_expr input =
    (chainl1 add_sub_expr
       (less_or_equal_op <|> more_or_equal_op <|> less_op <|> more_op
      <|> equal_op <|> not_equal_op))
      input

  and add_sub_expr input = (chainl1 mult_div_mod_expr (add_op <|> sub_op)) input

  and mult_div_mod_expr input =
    (chainl1 unaric_expr (mult_op <|> div_op <|> mod_op)) input

  and cast_expr input =
    ( parens define_type >>= fun cast_type ->
      unaric_expr >>= fun cast_expr -> return (Cast (cast_type, cast_expr)) )
      input

  and unaric_expr input =
    choice
      [
        (token "!" >> lexeme primary_expr >>= fun e -> return (Not e));
        ( token "-" >> lexeme primary_expr >>= fun e ->
          return (Sub (Value (VInt 0), e)) );
        (token "++" >> lexeme primary_expr >>= fun e -> return (PrefInc e));
        (token "--" >> lexeme primary_expr >>= fun e -> return (PrefDec e));
        (lexeme primary_expr >>= fun e -> token "++" >> return (PostInc e));
        (lexeme primary_expr >>= fun e -> token "--" >> return (PostDec e));
        lexeme primary_expr;
      ]
      input

  and primary_expr input =
    (cast_expr <|> class_creation <|> assign <|> access_by_point <|> call_method
   <|> this <|> base <|> parens expression <|> atomic)
      input

  and access_by_point input =
    let fold_access acc el = AccessByPoint (acc, el) in
    let called_parse =
      this <|> base <|> parens class_creation <|> call_method <|> identifier
      <|> parens cast_expr
    in
    let called_parse_continued =
      parens class_creation <|> call_method <|> identifier
    in
    ( called_parse >>= fun head ->
      many1 (token "." >> called_parse_continued) => fun tl ->
      List.fold_left fold_access head tl )
      input

  and split_by_comma input = sep_by expression (token ",") input

  and call method_identifier input =
    ( parens split_by_comma >>= fun list_expr ->
      return (CallMethod (method_identifier, list_expr)) )
      input

  and call_constructor input =
    (this <|> base >>= fun constr_identifier -> call constr_identifier) input

  and call_method input =
    (identifier >>= fun method_identifier -> call method_identifier) input

  and class_creation input =
    ( token "new" >> name >>= fun class_name ->
      parens split_by_comma >>= fun list_expr ->
      return (ClassCreation (class_name, list_expr)) )
      input

  and assign input =
    let left = access_by_point <|> identifier in
    ( left >>= fun left ->
      token "=" >> expression >>= fun right -> return (Assign (left, right)) )
      input

  let%test _ = apply atomic "    false" = Some (Value (VBool false))
  let%test _ = apply define_type " int" = Some TInt
  let%test _ = apply define_type "   Class  " = Some (TRef "Class")
  let%test _ = apply atomic "  14   " = Some (Value (VInt 14))
  let%test _ = apply define_type "  14, 19 ,99 " = None
end

module Statement = struct
  open Expression

  let break_stat = token "break" >> token ";" >> return Break

  let%test _ = apply break_stat "break;" = Some Break

  let continue_stat = token "continue" >> token ";" >> return Continue

  let%test _ = apply continue_stat "continue;" = Some Continue

  let return_stat =
    token "return"
    >> choice
         [
           ( skip_many1 space >> expression >>= fun ret ->
             token ";" >> return (Return (Some ret)) );
           token ";" >> return (Return None);
         ]

  let%test _ = apply return_stat "return;" = Some (Return None)

  let%test _ =
    apply return_stat "return 0;" = Some (Return (Some (Value (VInt 0))))

  let%test _ =
    apply return_stat "return x >= y;"
    = Some (Return (Some (MoreOrEqual (Identifier "x", Identifier "y"))))

  let expression_stat =
    expression >>= fun expr -> token ";" >> return (Expression expr)

  let%test _ =
    apply expression_stat "Call();"
    = Some (Expression (CallMethod (Identifier "Call", [])))

  let%test _ =
    apply expression_stat "Call(42, a, \"hi\");"
    = Some
        (Expression
           (CallMethod
              ( Identifier "Call",
                [ Value (VInt 42); Identifier "a"; Value (VString "hi") ] )))

  let%test _ =
    apply expression_stat "--m;" = Some (Expression (PrefDec (Identifier "m")))

  let rec statement input =
    choice
      [
        print_func;
        variable_decl;
        break_stat;
        continue_stat;
        return_stat;
        if_stat;
        while_stat;
        for_stat;
        expression_stat;
        statement_block;
      ]
      input

  and print_func input =
    ( token "Console.WriteLine(" >> expression >>= fun print_expression ->
      token ");" >> return (Print print_expression) )
      input

  and if_stat input =
    ( token "if" >> parens expression >>= fun condition ->
      statement >>= fun if_body ->
      choice
        [
          ( token "else" >> statement >>= fun else_body ->
            return (If (condition, if_body, Some else_body)) );
          return (If (condition, if_body, None));
        ] )
      input

  and statement_block input =
    ( token "{" >> sep_by statement spaces >>= fun block_stat ->
      token "}" >> return (StatementBlock block_stat) )
      input

  and while_stat input =
    ( token "while" >> parens expression >>= fun condition ->
      statement >>= fun while_body -> return (While (condition, while_body)) )
      input

  and variable_decl input =
    let helper =
      name >>= fun var_name ->
      token "=" >> expression
      >>= (fun var_value -> return (var_name, Some var_value))
      <|> return (var_name, None)
    in
    choice
      [
        ( const >>= fun modif ->
          define_type >>= fun var_type ->
          sep_by1 helper (token ",") >>= fun var_pair ->
          token ";" >> return (VariableDecl (Some modif, var_type, var_pair)) );
        ( define_type >>= fun var_type ->
          sep_by1 helper (token ",") >>= fun var_pair ->
          token ";" >> return (VariableDecl (None, var_type, var_pair)) );
      ]
      input

  and for_stat input =
    ( token "for"
    >> parens
         ( choice
             [
               (variable_decl >>= fun var_decl -> return (Some var_decl));
               token ";" >> return None;
             ]
         >>= fun declaration ->
           choice
             [
               (expression >>= fun expr -> token ";" >> return (Some expr));
               token ";" >> return None;
             ]
           >>= fun condition ->
           sep_by expression (token ",") >>= fun after_list ->
           return (declaration, condition, after_list) )
    >>= fun (declaration, condition, after_list) ->
      statement >>= fun body ->
      return (For (declaration, condition, after_list, body)) )
      input
end

module Object = struct
  open Expression
  open Statement

  let modifier =
    choice
      [
        token "public" >> return Public;
        token "private" >> return Private;
        token "protected" >> return Protected;
        token "const" >> return Const;
        token "virtual" >> return Virtual;
        token "override" >> return Override;
        token "abstract" >> return Abstract;
        token "static" >> return Static;
        token "new" >> return New;
      ]

  let%test _ =
    apply (many modifier) "public const virtual override abstract static new"
    = Some [ Public; Const; Virtual; Override; Abstract; Static; New ]

  let parameter =
    define_type >>= fun parameter_type ->
    name >>= fun parameter_name -> return (parameter_type, parameter_name)

  let%test _ =
    apply (sep_by parameter (token ",")) "int a, string b, Class o"
    = Some [ (TInt, Name "a"); (TString, Name "b"); (TRef "Class", Name "o") ]

  let method_decl =
    define_type >>= fun method_type ->
    choice
      [
        ( name >>= fun parent_name ->
          token "." >> name >>= fun method_name ->
          return (concatenate_name_with_sep "." parent_name method_name) );
        (name >>= fun method_name -> return method_name);
      ]
    >>= fun method_name ->
    parens (sep_by parameter (token ",")) >>= fun method_parameter_list ->
    choice
      [
        ( statement_block >>= fun method_statement_block ->
          return
            (Method
               ( method_type,
                 method_name,
                 method_parameter_list,
                 Some method_statement_block )) );
        token ";"
        >> return
             (Method (method_type, method_name, method_parameter_list, None));
      ]

  let constructor_decl =
    name >>= fun constr_name ->
    parens (sep_by parameter (token ",")) >>= fun constr_parameter_list ->
    choice
      [
        ( token ":" >> call_constructor >>= fun call_constr ->
          return (Some call_constr) );
        return None;
      ]
    >>= fun call_constr ->
    statement_block >>= fun constr_statement_block ->
    return
      (Constructor
         ( constr_name,
           constr_parameter_list,
           call_constr,
           constr_statement_block ))

  let field_decl =
    let variable_decl =
      name >>= fun field_name ->
      choice
        [
          ( token "=" >> expression >>= fun field_value ->
            return (field_name, Some field_value) );
          return (field_name, None);
        ]
    in
    define_type >>= fun field_type ->
    sep_by variable_decl (token ",") >>= fun variable_decl_list ->
    token ";" >> return (Field (field_type, variable_decl_list))

  (** Bool is_class argument used for substitution modifier to method (if class then private else public)*)
  let object_element is_class =
    many modifier >>= fun object_element_modifier_list ->
    let set_default_modifier =
      match object_element_modifier_list with
      | [] when is_class -> return [ Private ]
      | [] when not is_class -> return [ Public ]
      | modifier_list -> return modifier_list
    in
    set_default_modifier >>= fun updated_object_element_modifier_list ->
    field_decl <|> constructor_decl <|> method_decl >>= fun object_element ->
    return (updated_object_element_modifier_list, object_element)

  let%test _ =
    apply (object_element true) "public int Test(int a) { return a; }"
    = Some
        ( [ Public ],
          Method
            ( TInt,
              Name "Test",
              [ (TInt, Name "a") ],
              Some (StatementBlock [ Return (Some (Identifier "a")) ]) ) )

  let%test _ =
    apply (object_element true) "int Test(int a) { return a; }"
    = Some
        ( [ Private ],
          Method
            ( TInt,
              Name "Test",
              [ (TInt, Name "a") ],
              Some (StatementBlock [ Return (Some (Identifier "a")) ]) ) )

  let%test _ =
    apply (object_element false) "int Test(int a) { return a; }"
    = Some
        ( [ Public ],
          Method
            ( TInt,
              Name "Test",
              [ (TInt, Name "a") ],
              Some (StatementBlock [ Return (Some (Identifier "a")) ]) ) )

  let%test _ =
    apply (object_element true) "public int Class.Test(int a) { return a; }"
    = Some
        ( [ Public ],
          Method
            ( TInt,
              Name "Class.Test",
              [ (TInt, Name "a") ],
              Some (StatementBlock [ Return (Some (Identifier "a")) ]) ) )

  let%test _ =
    apply (object_element true) "public int Sum(int a, string b) {}"
    = Some
        ( [ Public ],
          Method
            ( TInt,
              Name "Sum",
              [ (TInt, Name "a"); (TString, Name "b") ],
              Some (StatementBlock []) ) )

  let%test _ =
    apply (object_element true) "public abstract int Sum(int a, string b);"
    = Some
        ( [ Public; Abstract ],
          Method
            (TInt, Name "Sum", [ (TInt, Name "a"); (TString, Name "b") ], None)
        )

  let%test _ =
    apply (object_element true) "public Win(Class o) {}"
    = Some
        ( [ Public ],
          Constructor
            (Name "Win", [ (TRef "Class", Name "o") ], None, StatementBlock [])
        )

  let%test _ =
    apply (object_element true) "public Win(Class o, string m) : base(m) {}"
    = Some
        ( [ Public ],
          Constructor
            ( Name "Win",
              [ (TRef "Class", Name "o"); (TString, Name "m") ],
              Some (CallMethod (Base, [ Identifier "m" ])),
              StatementBlock [] ) )

  let%test _ =
    apply (object_element true) "public static int test;"
    = Some ([ Public; Static ], Field (TInt, [ (Name "test", None) ]))

  let%test _ =
    apply (object_element true) "public const string mega = \"STR\";"
    = Some
        ( [ Public; Const ],
          Field (TString, [ (Name "mega", Some (Value (VString "STR"))) ]) )

  let object_decl_helper is_class =
    name >>= fun object_name ->
    choice
      [
        ( token ":" >> sep_by name (token ",") >>= fun parent_object_name ->
          return parent_object_name );
        return [];
      ]
    >>= fun parent_object_name ->
    token "{" >> sep_by (object_element is_class) spaces
    >>= fun object_element_list ->
    token "}" >> return (object_name, parent_object_name, object_element_list)

  let object_decl =
    many modifier >>= fun parsed_modifiers ->
    let substitute_mod_to_ast = function
      | [] ->
          choice
            [
              ( token "class" >> object_decl_helper true
              >>= fun (key, parent_key, element_list) ->
                return (Class ([ Public ], key, parent_key, element_list)) );
              ( token "interface" >> object_decl_helper false
              >>= fun (key, parent_key, element_list) ->
                return (Interface (Public, key, parent_key, element_list)) );
            ]
      | fst_modifier :: _ as object_modifier_list ->
          choice
            [
              ( token "class" >> object_decl_helper true
              >>= fun (key, parent_key, element_list) ->
                return
                  (Class (object_modifier_list, key, parent_key, element_list))
              );
              ( token "interface" >> object_decl_helper false
              >>= fun (key, parent_key, element_list) ->
                return (Interface (fst_modifier, key, parent_key, element_list))
              );
            ]
    in
    substitute_mod_to_ast parsed_modifiers

  let%test _ =
    apply object_decl "public abstract class Class : Parent{}"
    = Some (Class ([ Public; Abstract ], Name "Class", [ Name "Parent" ], []))

  let%test _ =
    apply object_decl "public  class Class\n{}"
    = Some (Class ([ Public ], Name "Class", [], []))

  let%test _ =
    apply object_decl "public  interface IInterface\n{}"
    = Some (Interface (Public, Name "IInterface", [], []))

  let%test _ =
    apply object_decl " class test : par1, par2, par3\n{}"
    = Some
        (Class
           ( [ Public ],
             Name "test",
             [ Name "par1"; Name "par2"; Name "par3" ],
             [] ))
end

let parser = many Object.object_decl
