(** Copyright 2022-2023, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
let spaces = skip_while is_space
let spaces = skip_many (satisfy is_space)
let str_integer = take_while1 (function '0' .. '9' -> true | _ -> false)
let integer = str_integer >>| int_of_string

let prohibited_words =
  [
    "for";
    "while";
    "if";
    "else";
    "break";
    "continue";
    "return";
    "do";
    "void";
    "int";
    "char";
    "int8_t";
    "int16_t";
    "int32_t";
    "float";
    "double";
  ]

let accepted_var_symbols = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

(* ------------------- *)
let cval x = Ast.Value x
let cvar x = Ast.Variable x

let carr arr_type name len elems =
  return (Ast.Array (arr_type, name, len, elems))

let carrelem name index = return (Ast.ArrayElem (name, index))
let cptr x = return (Ast.Pointer x)
let caddr x = return (Ast.Address x)
let cinc x = return (Ast.Inc x)
let cdec x = return (Ast.Dec x)
let cadd x y = return (Ast.Add (x, y))
let csub x y = return (Ast.Sub (x, y))
let cmul x y = return (Ast.Mul (x, y))
let cdiv x y = return (Ast.Div (x, y))
let cmod x y = return (Ast.Mod (x, y))
let cunarymin x = return (Ast.UnaryMin x)
let cunaryplus x = return (Ast.UnaryPlus x)
let ceq x y = return (Ast.Equal (x, y))
let cmore x y = return (Ast.More (x, y))
let cmoreeq x y = return (Ast.MoreOrEq (x, y))
let cnoteq x y = return (Ast.NotEqual (x, y))
let cless x y = return (Ast.Less (x, y))
let clesseq x y = return (Ast.LessOrEq (x, y))
let cand x y = return (Ast.And (x, y))
let cor x y = return (Ast.Or (x, y))
let ccast x y = return (Ast.Cast (x, y))
let casgn x y = return (Ast.Assign (x, y))
let cfcall x y = return (Ast.FuncCall (x, y))
let cdef x y z = return (Ast.Define (x, y, z))
let cdefseq exprs = return (Ast.DefineSeq exprs)
let cexpr input_expr = return (Ast.Expression input_expr)
let cstlist xs = return (Ast.StatementsBlock xs)
let cif condition stms = return (Ast.If (condition, stms))
let cifseq conditions parsed_else = return (Ast.IfSeq (conditions, parsed_else))
let cwhile condition stms = return (Ast.While (condition, stms))

let cfor first_expr condition last_expr stms =
  return (Ast.For (first_expr, condition, last_expr, stms))

let creturn expr = return (Ast.Return expr)

type dispatch = {
  func_call : dispatch -> Ast.expression Angstrom.t;
  arithmetical : dispatch -> string -> Ast.expression Angstrom.t;
  logical : dispatch -> string -> Ast.expression Angstrom.t;
  parse_and : dispatch -> string -> Ast.expression Angstrom.t;
  logical_sequence : dispatch -> string -> Ast.expression Angstrom.t;
  bracket : dispatch -> Ast.expression Angstrom.t;
  bracket_singles : dispatch -> string -> Ast.expression Angstrom.t;
  all_singles : dispatch -> string -> Ast.expression Angstrom.t;
  all_ops : dispatch -> string -> Ast.expression Angstrom.t;
  simple_commands : dispatch -> string -> Ast.expression Angstrom.t;
  statement : dispatch -> Ast.statement Angstrom.t;
  func_def : dispatch -> Ast.function_list Angstrom.t;
  statement_block : dispatch -> Ast.statement Angstrom.t;
}

type error = [ `ParsingError of string ]

let parse_name =
  let symbols_from_second s = accepted_var_symbols s || is_digit s in
  take_while1 accepted_var_symbols >>= fun first_symbol ->
  take_while symbols_from_second >>= fun other_symbols ->
  return (first_symbol ^ other_symbols)

let varname =
  parse_name >>= fun var_name ->
  if List.exists (fun x -> String.equal var_name x) prohibited_words then
    fail
      "Parsing error: Your variable name is equal to one of the keywords in C \
       language."
  else return (Ast.Variable var_name)

let pp_error ppf = function `ParsingError s -> Format.fprintf ppf "%s" s

let parse_const =
  [
    ( spaces *> integer <* spaces >>= fun c ->
      return (Ast.Value (Ast.VInt (Int32.of_int c))) );
    ( spaces *> char '\"' *> take_while (function '\"' -> false | _ -> true)
    <* char '\"' <* spaces
    >>= fun c -> return (Ast.Value (Ast.VString c)) );
    (spaces *> char '\'' *> peek_char >>= function
     | Some '\'' ->
         char '\'' *> spaces
         *> fail "quoted string should contain at least one character"
     | Some x ->
         char x *> char '\'' *> spaces *> return (Ast.Value (Ast.VChar x))
     | None -> fail "unclosed quote at the end of the file");
  ]

let rec foldi i f acc = if i <= 0 then acc else foldi (pred i) f (f acc)

let parse_type =
  let parse_simple_type =
    spaces *> parse_name <* spaces >>= function
    | "void" -> return Ast.TVoid
    | "int" -> return Ast.TInt32
    | "char" -> return Ast.TChar
    | "int8_t" -> return Ast.TInt8
    | "int16_t" -> return Ast.TInt16
    | "int32_t" -> return Ast.TInt32
    | _ -> fail "Parsing Error: unknown type."
  in
  parse_simple_type >>= fun expr ->
  many (spaces *> char '*' <* spaces) >>= fun ptrs ->
  return (foldi (List.length ptrs) (fun x -> Ast.TPointer x) expr)

let parse_inc =
  spaces *> varname <* spaces <* string "++" <* spaces >>= fun x ->
  casgn x (Ast.Add (x, Value (VInt 1l)))

let parse_dec =
  spaces *> varname <* spaces <* string "--" <* spaces >>= fun x ->
  casgn x (Ast.Sub (x, Value (VInt 1l)))

let parse_part_singles = choice (parse_const @ [ spaces *> varname <* spaces ])
let parse_singles = spaces *> parse_part_singles <* spaces

let parse_c =
  let input_end end_input =
    spaces *> peek_string (String.length end_input) >>= fun x ->
    match String.equal x end_input with
    | true -> return "Ok"
    | false -> fail "Parsing error: Wrong expression syntax."
  in
  let all_ops pack inp_end =
    spaces
    *> choice
         [
           pack.all_singles pack inp_end;
           pack.arithmetical pack inp_end;
           pack.logical pack inp_end;
           pack.logical_sequence pack inp_end;
           pack.parse_and pack inp_end;
         ]
    <* spaces
  in
  let rec make_args_list parsed_list acc =
    match parsed_list with
    | Ast.Variable "!None" :: tl -> make_args_list tl acc
    | expr :: tl -> make_args_list tl (acc @ [ expr ])
    | [] -> return acc
  in
  let func_call pack =
    fix (fun _ ->
        let func_all_ops inp_end =
          spaces
          *> choice
               [
                 return (Ast.Variable "!None") <* spaces <* input_end inp_end;
                 pack.all_ops pack inp_end;
               ]
          <* spaces
        in
        spaces *> parse_name >>= fun x ->
        char '(' *> many (func_all_ops "," <* char ',') >>= fun args ->
        make_args_list args [] >>= fun real_args ->
        func_all_ops ")"
        >>= (function
              | Ast.Variable "!None" -> return Option.None
              | arg -> return (Option.some arg))
        <* char ')' <* spaces
        >>= function
        | Some arg -> cfcall x (real_args @ [ arg ])
        | None -> cfcall x [])
  in
  let bracket_singles pack inp_end =
    let parse_arr_elem =
      fix (fun _ ->
          spaces *> parse_name >>= fun arr_name ->
          char '[' *> spaces *> pack.all_ops pack "]"
          <* spaces <* char ']' <* spaces <* input_end inp_end
          >>= fun elem_index -> carrelem arr_name elem_index)
    in
    let unaryminus =
      fix (fun _ ->
          spaces *> char '-' *> spaces *> pack.bracket_singles pack inp_end
          >>= fun x -> cunarymin x)
    in
    let unaryplus =
      fix (fun _ ->
          spaces *> char '+' *> spaces *> pack.bracket_singles pack inp_end
          >>= fun x -> cunaryplus x)
    in
    let pointer =
      fix (fun pointer ->
          spaces *> char '*' *> spaces
          *> (spaces *> varname <* spaces <* input_end inp_end
             <|> (pack.bracket pack <* input_end inp_end)
             <|> pointer)
          >>= fun x -> cptr x)
    in
    let address =
      fix (fun address ->
          spaces *> char '&' *> spaces
          *> (spaces *> varname <* spaces <* input_end inp_end
             <|> (pack.bracket pack <* input_end inp_end)
             <|> address)
          >>= fun x -> caddr x)
    in
    let parse_cast =
      fix (fun _ ->
          char '(' *> parse_type >>= fun x ->
          char ')'
          *> (pack.bracket pack <* input_end inp_end
             <|> pack.bracket_singles pack inp_end)
          >>= fun y -> ccast x y)
    in
    spaces
    *> choice
         [
           parse_arr_elem <* spaces;
           unaryminus <* spaces;
           unaryplus <* spaces;
           pointer <* spaces;
           address <* spaces;
           parse_inc <* input_end inp_end;
           parse_dec <* input_end inp_end;
           parse_singles <* input_end inp_end;
           pack.bracket pack <* input_end inp_end;
           pack.func_call pack <* input_end inp_end;
           parse_cast <* input_end inp_end;
         ]
    <* spaces
  in
  let all_singles pack end_input =
    let parse_priority =
      pack.bracket_singles pack "*"
      <|> pack.bracket_singles pack "/"
      <|> pack.bracket_singles pack "%"
      >>= fun left ->
      choice [ string "*"; string "/"; string "%" ] >>= fun operator ->
      pack.bracket_singles pack end_input
      <|> spaces *> pack.all_singles pack end_input
      >>= fun right ->
      let create_expr =
        match operator with
        | "*" -> cmul left right
        | "/" -> cdiv left right
        | "%" -> cmod left right
        | _ -> fail "Not priority operator"
      in
      create_expr
    in
    spaces
    *> (pack.bracket_singles pack end_input
       <|> parse_priority <* input_end end_input)
    <* spaces
  in
  let arithmetical pack end_input =
    fix (fun _ ->
        let parse_simple_op_without_brackets end_input op =
          pack.all_singles pack op <* string op >>= fun left ->
          pack.all_singles pack end_input <* input_end end_input
          >>= fun right ->
          let create_expr =
            match op with
            | "+" -> cadd left right
            | "-" -> csub left right
            | _ -> fail "op is not + or - operator"
          in
          create_expr
        in
        let parse_simple_op end_input op =
          parse_simple_op_without_brackets end_input op :: []
        in
        let parse_simple_ops end_input =
          parse_simple_op end_input "+" @ parse_simple_op end_input "-"
        in
        let parse_complex_op end_input operator =
          match operator with
          | "++" ->
              spaces *> varname <* spaces <* string "++" <* input_end end_input
              >>= fun x -> cinc x
          | "--" ->
              spaces *> varname <* spaces <* string "--" <* input_end end_input
              >>= fun x -> cdec x
          | _ ->
              pack.all_singles pack operator <* string operator >>= fun left ->
              let matching = function
                | Ast.Variable _ -> spaces *> pack.arithmetical pack end_input
                | Ast.Value _ -> spaces *> pack.arithmetical pack end_input
                | _ ->
                    spaces *> pack.all_singles pack end_input
                    <|> spaces *> pack.arithmetical pack end_input
              in
              matching left >>= fun right ->
              let create_expr =
                match operator with
                | "+" -> cadd left right
                | "-" -> csub left right
                | _ -> fail "op is not + or - operator"
              in
              create_expr
        in
        choice
          (parse_simple_ops end_input
          @ [
              parse_complex_op end_input "+";
              parse_complex_op end_input "-";
              parse_complex_op end_input "++";
              parse_complex_op end_input "--";
            ]))
  in
  let logical pack inp_end =
    fix (fun _ ->
        let parse_logical_op end_input operator =
          pack.all_singles pack operator
          <|> pack.arithmetical pack operator
          <* spaces <* string operator
          >>= fun left ->
          pack.all_singles pack end_input
          <|> pack.arithmetical pack inp_end
          <|> pack.logical pack inp_end <* spaces
          >>= fun right ->
          match operator with
          | "==" -> ceq left right
          | ">" -> cmore left right
          | ">=" -> cmoreeq left right
          | "<" -> cless left right
          | "<=" -> clesseq left right
          | "!=" -> cnoteq left right
          | _ -> fail "operator is not a simple logical operator"
        in
        let parse_logical end_input =
          [
            parse_logical_op end_input "==";
            parse_logical_op end_input ">=";
            parse_logical_op end_input ">";
            parse_logical_op end_input "<=";
            parse_logical_op end_input "<";
            parse_logical_op end_input "!=";
          ]
        in
        choice (parse_logical inp_end))
  in
  let parse_and pack inp_end =
    fix (fun _ ->
        let parse_complex_ops end_input =
          pack.arithmetical pack end_input <|> pack.logical pack end_input
        in
        let parse_and end_inp operator =
          pack.all_singles pack operator
          <|> parse_complex_ops operator <* spaces <* string operator
          >>= fun left ->
          pack.all_singles pack end_inp
          <|> spaces *> parse_complex_ops end_inp
          <|> pack.parse_and pack inp_end
          <* spaces
          >>= fun right ->
          match operator with
          | "&&" -> cand left right
          | _ -> fail "operator is not && operator"
        in
        parse_and inp_end "&&")
  in
  let logical_sequence pack inp_end =
    fix (fun _ ->
        let parse_complex_ops end_input =
          pack.arithmetical pack end_input <|> pack.logical pack end_input
        in
        let parse_or end_inp operator =
          pack.all_singles pack operator
          <|> parse_complex_ops operator
          <|> pack.parse_and pack operator
          <* spaces <* string operator
          >>= fun left ->
          pack.all_singles pack end_inp
          <|> spaces *> parse_complex_ops end_inp
          <|> pack.parse_and pack inp_end
          <|> pack.logical_sequence pack inp_end
          <* spaces
          >>= fun right ->
          match operator with
          | "&&" -> cand left right
          | "||" -> cor left right
          | _ -> fail "operator is not a && or || operator"
        in
        parse_or inp_end "||")
  in
  let bracket pack =
    fix (fun _ ->
        spaces *> char '('
        *> choice
             [
               pack.all_singles pack ")";
               pack.arithmetical pack ")";
               pack.logical pack ")";
               pack.logical_sequence pack ")";
               pack.parse_and pack ")";
               parse_singles <* input_end ")";
             ]
        <* char ')' <* spaces)
  in
  let simple_commands pack inp_end =
    fix (fun _ ->
        let parse_ops =
          pack.logical_sequence pack inp_end
          <|> pack.parse_and pack inp_end
          <|> pack.logical pack inp_end
          <|> pack.arithmetical pack inp_end
          <|> pack.all_singles pack inp_end
        in
        let parse_arithmetical_commands =
          spaces *> varname <* spaces >>= fun left ->
          choice
            [ string "+="; string "-="; string "*="; string "/="; string "%=" ]
          <* spaces
          >>= fun operator ->
          pack.all_ops pack inp_end >>= fun right ->
          match operator with
          | "+=" -> cadd left right >>= fun op -> casgn left op
          | "-=" -> csub left right >>= fun op -> casgn left op
          | "*=" -> cmul left right >>= fun op -> casgn left op
          | "/=" -> cdiv left right >>= fun op -> casgn left op
          | "%=" -> cmod left right >>= fun op -> casgn left op
          | _ -> fail "operator is not a shorter arithmetical operator"
        in
        let parse_array =
          char '{' *> many (pack.all_ops pack "," <* char ',') >>= fun args ->
          pack.all_ops pack "}"
          >>= (fun arg -> return (Option.some arg))
          <|> spaces *> input_end "}" *> return Option.None
          <* char '}' <* spaces <* input_end inp_end
          >>= function
          | Some arg -> return (Option.some (args @ [ arg ]))
          | None -> return Option.None
        in
        let parse_array_def =
          spaces *> parse_type <* spaces
          >>= (fun arr_type ->
                spaces *> parse_name >>= fun arr_name ->
                char '['
                *> ( pack.all_ops pack "]" >>= fun x ->
                     return (Option.some x)
                     <|> spaces *> input_end "]" *> return Option.None )
                <* char ']' <* spaces
                >>= fun len ->
                char '=' *> spaces *> parse_array >>= fun arr_body ->
                carr arr_type arr_name len arr_body)
          <|> ( spaces *> parse_type <* spaces >>= fun arr_type ->
                spaces *> parse_name >>= fun arr_name ->
                char '['
                *> ( pack.all_ops pack "]" >>= fun x ->
                     return (Option.some x)
                     <|> spaces *> input_end "]" *> return Option.None )
                <* char ']' <* spaces <* input_end inp_end
                >>= fun len -> carr arr_type arr_name len Option.None )
        in
        let parse_assign =
          spaces *> varname <* spaces <* input_end "="
          <|> (pack.bracket_singles pack "=" >>= function
               | Pointer x -> return (Ast.Pointer x)
               | ArrayElem (x, y) -> carrelem x y
               | _ -> fail "Trying to assign value to an l-value")
          >>= fun x ->
          char '=' *> spaces *> parse_ops <* input_end inp_end >>= fun y ->
          casgn x y
        in
        let parse_definition =
          let parse_simple_def vars_type end_input =
            spaces *> varname <* spaces >>= fun var_name ->
            char '=' *> spaces *> pack.all_ops pack end_input
            <* spaces
            >>= (fun var_def -> cdef vars_type var_name (Option.some var_def))
            <|> input_end end_input *> spaces
                *> cdef vars_type var_name Option.None
          in
          spaces *> parse_type <* spaces >>= fun vars_type ->
          many (parse_simple_def vars_type "," <* char ',') >>= fun vars_defs ->
          parse_simple_def vars_type inp_end >>= fun last_def ->
          cdefseq (vars_defs @ [ last_def ])
        in
        let parse_allowed_singles =
          spaces *> pack.bracket_singles pack inp_end >>= function
          | Ast.Cast (x, y) -> ccast x y
          | Ast.FuncCall (name, params) -> cfcall name params
          | _ -> fail "expr is not allowed to be a single command"
        in
        choice
          [
            parse_arithmetical_commands;
            parse_array_def;
            parse_assign;
            parse_definition;
            parse_allowed_singles;
          ])
  in
  let statement_block pack =
    char '{' *> spaces
    *> many
         (pack.statement pack
         <|> ( spaces *> string "return" *> spaces *> pack.all_ops pack ";"
             <* char ';'
             >>= fun x -> creturn x ))
    <* spaces <* char '}' <* spaces
    >>= fun x -> cstlist x
  in
  let statement pack =
    fix (fun _ ->
        let parse_simple_statement =
          spaces *> pack.simple_commands pack ";" <* char ';' <* spaces
          >>= fun x -> cexpr x
        in
        let parse_ifs beginning =
          string beginning *> spaces *> char '(' *> pack.all_ops pack ")"
          <* spaces <* char ')' <* spaces
          >>= fun condition ->
          pack.statement_block pack >>= fun stms -> cif condition stms
        in
        let parse_elif = many (spaces *> parse_ifs "else if " <* spaces) in
        let parse_else =
          spaces *> string "else" *> spaces *> pack.statement_block pack
          >>= fun stms -> return (Option.some stms) <* spaces
        in
        let parse_if =
          parse_ifs "if " >>= fun fst_if ->
          parse_elif >>= fun elifs ->
          parse_else <|> return Option.None >>= fun parsed_else ->
          cifseq ([ fst_if ] @ elifs) parsed_else
        in
        let parse_break =
          spaces *> string "break" *> spaces *> char ';' *> return Ast.Break
        in
        let parse_continue =
          spaces *> string "break" *> spaces *> char ';' *> return Ast.Continue
        in
        let parse_loop_statement_block =
          char '{' *> spaces
          *> many (parse_break <|> parse_continue <|> pack.statement pack)
          <* spaces <* char '}' <* spaces
          >>= fun x -> cstlist x
        in
        let parse_while =
          string "while" *> spaces *> char '(' *> pack.all_ops pack ")"
          <* spaces <* char ')' <* spaces
          >>= fun condition ->
          parse_loop_statement_block >>= fun stms -> cwhile condition stms
        in
        let parse_option end_symbol =
          choice
            [
              pack.all_ops pack end_symbol; pack.simple_commands pack end_symbol;
            ]
          >>= fun expr -> return (Option.some expr)
        in
        let parse_for =
          string "for" *> spaces *> char '(' *> spaces
          *> (parse_option ";" <|> return Option.None)
          <* spaces <* char ';'
          >>= fun first_expr ->
          spaces
          *> (pack.all_ops pack ";"
             >>= (fun expr -> return (Option.Some expr))
             <|> return Option.None)
          <* spaces <* char ';' <* spaces
          >>= fun condition ->
          spaces *> (parse_option ")" <|> return Option.None)
          <* spaces <* char ')' <* spaces
          >>= fun loop_expr ->
          parse_loop_statement_block >>= fun stms ->
          cfor first_expr condition loop_expr stms
        in
        choice [ parse_simple_statement; parse_if; parse_while; parse_for ])
  in
  let parse_arg inp_end =
    spaces *> parse_type <* spaces >>= fun arg_type ->
    parse_name <* spaces <* input_end inp_end >>= fun arg_name ->
    return (arg_type, arg_name)
  in
  let func_def pack =
    spaces
    *> many
         ( parse_type >>= fun func_type ->
           spaces *> parse_name >>= fun func_name ->
           if List.exists (fun x -> String.equal func_name x) prohibited_words
           then
             fail
               "Parsing Error: Your function name is equal to one of the \
                keywords in C language."
           else
             char '(' *> many (parse_arg "," <* char ',') >>= fun args ->
             parse_arg ")"
             >>= (fun arg -> return (Option.some arg))
             <|> return Option.None <* char ')' <* spaces
             >>= fun last_arg ->
             pack.statement_block pack
             >>= (fun stms -> return (Option.some stms))
             <|> return Option.None
             >>= fun func_body ->
             match last_arg with
             | Some arg ->
                 return
                   {
                     Ast.function_type = func_type;
                     Ast.function_name = func_name;
                     Ast.function_arguments = args @ [ arg ];
                     Ast.function_body = func_body;
                   }
             | None ->
                 return
                   {
                     Ast.function_type = func_type;
                     Ast.function_name = func_name;
                     Ast.function_arguments = [];
                     Ast.function_body = func_body;
                   } )
    <* spaces
  in
  {
    func_call;
    arithmetical;
    logical;
    parse_and;
    logical_sequence;
    bracket;
    bracket_singles;
    all_singles;
    all_ops;
    simple_commands;
    statement;
    func_def;
    statement_block;
  }

let parse str =
  match
    Angstrom.parse_string (parse_c.func_def parse_c)
      ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`ParsingError er)
