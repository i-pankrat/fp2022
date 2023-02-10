(** Copyright 2022-2023, Vladislav Shalnev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open InterpretMonadFailInf

module Result : MONAD_FAIL with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return = Result.ok
  let fail = Result.error
  let ( >>= ) = Result.bind
  let ( >>| ) f g = f >>= fun x -> return (g x)
  let ( *> ) f g = f >>= fun _ -> return g
  let ( <* ) f g = f >>= fun e -> return g >>= fun _ -> return e
  let ( let* ) = ( >>= )
  let ( let** ) = ( >>| )

  let ( <|> ) = function
    | Error _ -> Fun.id
    | f -> Fun.const f
  ;;

  let choice l = List.fold_left ( <|> ) (fail "No more choices") l
end

module Stack : sig
  type 'a t

  val singleton : 'a -> 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end = struct
  type 'a t = 'a list

  let singleton a = [ a ]
  let is_empty l = l = []
  let push = List.cons
  let peek = List.hd
  let pop = List.tl
  let pp pp_v ppf = List.iter (Format.fprintf ppf "@[%a@] " pp_v)
end

module StringMap : sig
  include Map.S with type key = string

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.(iter (fun k -> fprintf ppf "@[%a=%a@] " pp_print_string k pp_v) m)
  ;;
end

module Eval (M : MONAD_FAIL) = struct
  open M
  open Ast

  type 'a p_value =
    | LiteralValue of Ast.literal
    | ArrayValue of 'a p_value list
    | ClassValue of 'a p_value StringMap.t
    | MethodValue of string * (string list * Ast.t)
    | LambdaValue of 'a * (string list * Ast.t)
    | InstanceValue of 'a p_value StringMap.t
  [@@deriving variants, show { with_path = false }]

  (* State contains info about local variables (vars) and stack of class scopes *)
  type state =
    { vars : state p_value StringMap.t
    ; classes : state p_value StringMap.t Stack.t
    }
  [@@deriving show { with_path = false }]

  type value = state p_value [@@deriving show { with_path = false }]

  type env =
    { state : state
    ; ret : value
    }

  let get_var { vars; classes } name =
    StringMap.find_opt name vars
    |> function
    | Some var -> return var
    | None ->
      let rec find_in_stack = function
        | stack when Stack.is_empty stack ->
          fail (Format.sprintf "undefined local variable or method '%S'" name)
        | stack ->
          (match StringMap.find_opt name (Stack.peek stack) with
           | Some v -> return v
           | None -> find_in_stack (Stack.pop stack))
      in
      find_in_stack classes
  ;;

  let default_state =
    { vars = StringMap.empty; classes = Stack.singleton StringMap.empty }
  ;;

  let default_ret = literalvalue nillit
  let default_env = { state = default_state; ret = default_ret }

  let set_var ?(to_class = false) { vars; classes } key value =
    match key.[0] with
    | a when to_class || a = '@' ->
      let classes = Stack.(push (StringMap.add key value (peek classes)) (pop classes)) in
      { vars; classes }
    | _ ->
      let vars = StringMap.add key value vars in
      { vars; classes }
  ;;

  let reset_vars state = { state with vars = StringMap.empty }

  (* Push class state to stack *)
  let push ?(c = StringMap.empty) state =
    { state with classes = Stack.push c state.classes }
  ;;

  (* Peek class state *)
  let peek { vars = _; classes } = Stack.peek classes

  (** Error for undefined method *)
  let undefined_method op lhs rhs =
    match op with
    | EQ -> return (boollit false)
    | NEQ -> return (boollit true)
    | op ->
      fail (Format.sprintf "Undefined method %S for (%S, %S)" (show_binop op) lhs rhs)
  ;;

  (** Error for no method *)
  let no_method self =
    fail
      (Format.sprintf
         "undefined method '%S' for '%S' (NoMethodError)"
         "new"
         (show_value self))
  ;;

  let eval_lit_binop = function
    | IntLit x, IntLit y ->
      let to_lit lit op = return (lit (op x y)) in
      (function
       | ADD -> to_lit intlit ( + )
       | SUB -> to_lit intlit ( - )
       | MULT -> to_lit intlit ( * )
       | DIV -> if y = 0 then fail "Divide by zero" else to_lit intlit ( / )
       | MOD -> to_lit intlit ( mod )
       | EQ -> to_lit boollit ( = )
       | NEQ -> to_lit boollit ( != )
       | GTR -> to_lit boollit ( > )
       | GEQ -> to_lit boollit ( >= )
       | LSS -> to_lit boollit ( < )
       | LEQ -> to_lit boollit ( <= )
       | op -> undefined_method op "Int" "Int")
    | StringLit x, StringLit y ->
      let to_bool_lit op = return (boollit (op x y)) in
      (function
       | ADD -> return (stringlit (x ^ y))
       | EQ -> to_bool_lit String.equal
       | NEQ -> to_bool_lit (fun x y -> not (String.equal x y))
       | GTR -> to_bool_lit (fun x y -> String.compare x y > 0)
       | GEQ -> to_bool_lit (fun x y -> String.compare x y >= 0)
       | LSS -> to_bool_lit (fun x y -> String.compare x y < 0)
       | LEQ -> to_bool_lit (fun x y -> String.compare x y <= 0)
       | op -> undefined_method op "String" "String")
    | StringLit x, IntLit y ->
      let to_str_lit op = return (stringlit (op x y)) in
      (function
       | MULT -> to_str_lit (fun x n -> String.concat "" (List.init n (Fun.const x)))
       | op -> undefined_method op "String" "Int")
    | BoolLit x, BoolLit y ->
      let to_bool_lit op = return (boollit (op x y)) in
      (function
       | EQ -> to_bool_lit ( = )
       | NEQ -> to_bool_lit ( != )
       | AND -> to_bool_lit ( && )
       | OR -> to_bool_lit ( || )
       | op -> undefined_method op "Bool" "Bool")
    | NilLit, NilLit ->
      (function
       | EQ -> return (BoolLit true)
       | NEQ -> return (BoolLit false)
       | op -> undefined_method op "Nil" "Nil")
    | x, y -> fun op -> undefined_method op (show_literal x) (show_literal y)
  ;;

  let eval_binop = function
    | LiteralValue lhs, LiteralValue rhs ->
      fun op -> eval_lit_binop (lhs, rhs) op >>| literalvalue
    | ArrayValue x, ArrayValue y ->
      let to_lit lit op = return (lit (op x y)) in
      (function
       | ADD -> to_lit arrayvalue ( @ )
       | EQ -> to_lit boollit ( = ) >>| literalvalue
       | NEQ -> to_lit boollit ( != ) >>| literalvalue
       | op ->
         fail (Format.sprintf "Undefined method %S for BoolLit, BoolLit" (show_binop op)))
    | ArrayValue x, LiteralValue (IntLit y) ->
      let to_lit lit op = return (lit (op x y)) in
      (function
       | MULT -> to_lit arrayvalue (fun x n -> List.concat (List.init n (Fun.const x)))
       | op ->
         fail (Format.sprintf "Undefined method %S for BoolLit, BoolLit" (show_binop op)))
    | x, y -> fun op -> undefined_method op (show_value x) (show_value y) >>| literalvalue
  ;;

  (* 'r' in name means 'reversed' *)
  let as_ints_r =
    List.fold_left
      (fun acc -> function
        | LiteralValue (IntLit i) -> acc >>| List.cons i
        | a -> fail (Format.sprintf "Expected int, but received '%S'" (show_value a)))
      (return [])
  ;;

  let as_ints l = as_ints_r l >>| List.rev

  let as_int l =
    as_ints l
    >>= fun vs ->
    if List.length vs = 1 then return (List.hd vs) else fail "Wrong number of arguments"
  ;;

  let rec eval ?(state = default_state) =
    let eval_many_r ~state =
      List.fold_left
        (fun acc cur ->
          let* state, arr = acc in
          eval ~state cur >>| fun { state; ret } -> state, ret :: arr)
        (return (state, []))
    in
    let eval_many ~state exprs =
      eval_many_r ~state exprs >>| fun (state, values) -> state, List.rev values
    in
    let eval_as_bool ~state c =
      let* { state; ret } = eval ~state c in
      match ret with
      | LiteralValue (BoolLit c) -> return (state, c)
      | ret -> fail (Format.sprintf "'%S' in condition" (show_value ret))
    in
    function
    | Literal lit -> return { state; ret = literalvalue lit }
    | Var name -> get_var state name >>| fun ret -> { state; ret }
    | Assn (name, e) ->
      eval ~state e >>| fun env -> { env with state = set_var state name env.ret }
    | SeqCompound seq ->
      List.fold_left
        (fun acc cur -> acc >>= fun { state; _ } -> eval ~state cur)
        (return { default_env with state })
        seq
    | Binop (op, x, y) ->
      let* { state; ret = xret } = eval ~state x in
      let* { state; ret = yret } = eval ~state y in
      eval_binop (xret, yret) op >>| fun ret -> { state; ret }
    | IfElse (c, e1, e2) ->
      eval_as_bool ~state c >>= fun (state, cond) -> eval ~state (if cond then e1 else e2)
    | WhileCompound (c, s) as w ->
      eval_as_bool ~state c
      >>= (function
      | state, true -> eval ~state s >>= fun { state; _ } -> eval ~state w
      | state, false -> return { state; ret = default_ret })
    | UntilCompound (c, s) ->
      eval ~state s >>= fun { state; _ } -> eval ~state (whilecompound c s)
    | Arr exprs ->
      eval_many ~state exprs >>| fun (state, values) -> { state; ret = arrayvalue values }
    | MethodFn (name, body) ->
      let ret = methodvalue name body in
      return { state = set_var ~to_class:true state name ret; ret }
    | LambdaFn (params, body) -> return { state; ret = lambdavalue state (params, body) }
    | MethodCall (this, (name, params)) ->
      let init_state = state in
      let* state, values = eval_many ~state params in
      let* { state; ret = this_v } = eval ~state this in
      (match name, this_v with
       | "new", ClassValue class_state ->
         eval_new ~state class_state values
         >>| fun (ret, _) -> { ret; state = init_state }
       | name, InstanceValue class_state ->
         eval_method ~state name this class_state values
       | _ -> eval_builtin name this_v values >>| fun ret -> { state; ret })
    | ClassDecl (name, body) ->
      let state = push (reset_vars state) in
      let** upd_state, r_values = eval_many_r ~state body in
      { state = set_var state name (classvalue (peek upd_state)); ret = List.hd r_values }
    | Invoke { invoker; args } ->
      let eval_fn init_state values =
        let inner state (params, body) =
          let state = List.fold_left2 set_var state params values in
          eval ~state body >>| fun { ret; _ } -> { state = init_state; ret }
        in
        function
        | MethodValue (name, p) as fn -> inner (set_var (reset_vars init_state) name fn) p
        | LambdaValue (state, p) -> inner state p
        | v -> fail (Format.sprintf "Undefined method for '%S'" (show_value v))
      in
      let* { state; ret = invoker } = eval ~state invoker in
      let* state, values = eval_many ~state args in
      eval_fn state values invoker

  and eval_new ~state class_state params =
    (* Push state to stack from ClassValue and reset vars *)
    let state = push ~c:class_state (reset_vars state) in
    get_var state "initialize"
    >>= function
    | MethodValue (_, p) ->
      let eval_fn state values (args, body) =
        let state = List.fold_left2 set_var state args values in
        eval ~state body >>| fun { state; _ } -> peek state
      in
      eval_fn state params p >>| fun x -> instancevalue x, peek state
    | _ -> no_method (ClassValue class_state)

  and eval_method ~state name self class_state params =
    let init_state = state in
    let upd_state = push ~c:class_state (reset_vars state) in
    get_var upd_state name
    >>= function
    | MethodValue (_, p) ->
      let eval_fn state values (args, body) =
        let state = List.fold_left2 set_var state args values in
        let** { state; ret } = eval ~state body in
        match self with
        | Var name ->
          let state = set_var init_state name (instancevalue (peek state)) in
          { state; ret }
        | _ -> { state; ret }
      in
      eval_fn upd_state params p
    | _ -> no_method (instancevalue class_state)

  and eval_builtin name self params =
    let wr_args exp =
      fail
        (Format.sprintf
           "wrong number of arguments (expected '%S', received '%i') (ArgumentError)"
           exp
           (List.length params))
    in
    match name with
    | "[]" ->
      (match self with
       | LiteralValue (IntLit a) ->
         as_int params
         >>| fun id -> literalvalue (intlit (Int.logand 1 (Int.shift_right a id)))
       | LiteralValue (StringLit s) ->
         as_ints params
         >>= (function
         | [ pos ] -> return (literalvalue (stringlit (String.sub s pos 1)))
         | [ pos; len ] -> return (literalvalue (stringlit (String.sub s pos len)))
         | _ -> wr_args "1..2")
       | ArrayValue vs ->
         choice
           [ as_int params >>| List.nth vs
           ; (as_ints_r params
             >>| fun ids ->
             arrayvalue (List.fold_left (fun acc id -> List.nth vs id :: acc) [] ids))
           ; wr_args "1..2"
           ]
       | _ -> no_method self)
    | "length" ->
      (match self with
       | LiteralValue (StringLit s) -> return (literalvalue (intlit (String.length s)))
       | ArrayValue l -> return (literalvalue (intlit (List.length l)))
       | _ -> no_method self)
    | _ -> no_method self
  ;;

  let run input =
    match Parser.parse input with
    | Error err -> fail err
    | Ok ast -> eval ast
  ;;
end

(** Tests **)

open Eval (Result)

let test =
  let inner input =
    match Parser.parse input with
    | Ok ast -> eval ast
    | Error e -> Result.fail (Format.sprintf "Parsing error (%S)" e)
  in
  fun input ->
    match inner input with
    | Ok { ret; _ } -> pp_value Format.std_formatter ret
    | Error e -> print_endline e
;;

(* Tests added during development, other tests in demos/interpretTests.t *)

let%expect_test _ =
  test {|10|};
  [%expect {|(LiteralValue (IntLit 10)) |}]
;;

let%expect_test _ =
  test {|10+0|};
  [%expect {|(LiteralValue (IntLit 10))|}]
;;

let%expect_test _ =
  test {|10+1|};
  [%expect {|(LiteralValue (IntLit 11))|}]
;;

let%expect_test _ =
  test {|10==2|};
  [%expect {|(LiteralValue (BoolLit false))|}]
;;

let%expect_test _ =
  test {|10 + 227 * 2 / 2 > 5|};
  [%expect {|(LiteralValue (BoolLit true))|}]
;;

let%expect_test _ =
  test {|1+2
2+3|};
  [%expect {|(LiteralValue (IntLit 5))|}]
;;

let%expect_test _ =
  test {|if false then 2 else 'what' end|};
  [%expect {|(LiteralValue (StringLit "what"))|}]
;;

let%expect_test _ =
  test {|if true then 39 else 'what' end|};
  [%expect {|(LiteralValue (IntLit 39))|}]
;;

let%expect_test _ =
  test {|x=10|};
  [%expect {|(LiteralValue (IntLit 10))|}]
;;

let%expect_test _ =
  test {|x=10;x+2|};
  [%expect {|(LiteralValue (IntLit 12))|}]
;;

let%expect_test _ =
  test {|t=1;
    a=[10,10+4,true,t]|};
  [%expect
    {|
    (ArrayValue
       [(LiteralValue (IntLit 10)); (LiteralValue (IntLit 14));
         (LiteralValue (BoolLit true)); (LiteralValue (IntLit 1))])|}]
;;

let%expect_test _ =
  test {|x=0;while x < 5 do x=x+1 end|};
  [%expect {|(LiteralValue NilLit)|}]
;;

let%expect_test _ =
  test {|x=0;while x < 5 do x=x+1 end;x|};
  [%expect {|(LiteralValue (IntLit 5))|}]
;;

let%expect_test _ =
  test {|x=0;while x < 5 do x=x+1; x=x+20 end;x|};
  [%expect {|(LiteralValue (IntLit 21))|}]
;;

let%expect_test _ =
  test {|3[2]|};
  [%expect {|(LiteralValue (IntLit 0))|}]
;;

let%expect_test _ =
  test {|3[1]|};
  [%expect {|(LiteralValue (IntLit 1))|}]
;;

let%expect_test _ =
  test {|3[0]|};
  [%expect {|(LiteralValue (IntLit 1))|}]
;;

let%expect_test _ =
  test {|[1,2][0]|};
  [%expect {|(LiteralValue (IntLit 1))|}]
;;

let%expect_test _ =
  test {|[1,2][1]|};
  [%expect {|(LiteralValue (IntLit 2))|}]
;;

let%expect_test _ =
  test {|[1,true][1]|};
  [%expect {|(LiteralValue (BoolLit true))|}]
;;

let%expect_test _ =
  test {|[1,true,'str'][2,0,1]|};
  [%expect
    {|
    (ArrayValue
       [(LiteralValue (StringLit "str")); (LiteralValue (IntLit 1));
         (LiteralValue (BoolLit true))])|}]
;;

let%expect_test _ =
  test {|'str'.length()|};
  [%expect {|(LiteralValue (IntLit 3))|}]
;;

let%expect_test _ =
  test {|[1,true,'str'].length()|};
  [%expect {|(LiteralValue (IntLit 3))|}]
;;

let%expect_test _ =
  test {|def methodname()
    a=10
end
methodname()|};
  [%expect {|(LiteralValue (IntLit 10))|}]
;;

let%expect_test _ =
  test {|b=2
  y=lambda {|x|1+b}
  y(10)|};
  [%expect {|(LiteralValue (IntLit 3))|}]
;;

let%expect_test _ =
  test
    {|class Pair
          def initialize(a,b)
              @b=a
              @a=b
          end
          def get_a()
              @b
          end
          def get_b()
              @a
          end
      end|};
  [%expect {|
    (MethodValue ("get_b", ([], (Var "@a"))))|}]
;;

let%expect_test _ =
  test
    {|class Pair
          def initialize(a,b)
              @b=a
              @a=b
          end
          def get_a()
              @b
          end
          def get_b()
              @a
          end
      end
      p=Pair.new(2,3)
      [p.get_a(), p.get_b()]|};
  [%expect {| (ArrayValue [(LiteralValue (IntLit 2)); (LiteralValue (IntLit 3))])|}]
;;
