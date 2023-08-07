include Token

module Boolean = struct
  type t = {
    token : Token.t;
    value : bool;
  }
  [@@deriving show]

  let of_token (token : Token.t) = 
    match token.literal with 
    "true" -> {token; value = true}
    | "false" -> {token; value = false}
    | _ -> 
      let error_msg = Printf.sprintf "cannot init Boolean with token %s" token.literal in
      failwith error_msg
  ;;

  let eq (a : t) (b : t) : bool = 
    Printf.printf "a: {Token: (%s) value: %s}" (Token.string_of a.token) (string_of_bool a.value);
    Printf.printf "b: {Token: (%s) value: %s}" (Token.string_of a.token) (string_of_bool a.value);
    print_endline "eq";
    print_endline (string_of_bool (Token.eq a.token b.token));
    print_endline (string_of_bool (a.value = b.value));
    match (a, b) with
      | ({token = a_token; value = a_value}, {token = b_token; value = b_value}) -> 
        (Token.eq a_token b_token) && (a_value = b_value)
  ;;

  let string_of b = string_of_bool b.value;;

  let string_of_complete id = 
    Printf.sprintf "Boolean: {Token: (%s) value: %s}" (Token.string_of id.token) (string_of_bool id.value)
  ;;
  let print stmt = 
    Printf.sprintf "%b" stmt.value
  ;;
end

module Identifier = struct
  type t = {
    token : Token.t;
    name : string;
  }
  [@@deriving show]

  let of_token token = 
    {token; name = token.literal}
  ;;

  let eq (a : t) (b : t) : bool = 
    match (a, b) with
      | ({token = a_token; name = a_name}, {token = b_token; name = b_name}) -> 
        (Token.eq a_token b_token) && (a_name = b_name)
  ;;

  let string_of id = id.name;;

  let string_of_complete id = 
    Printf.sprintf "Identifier: {Token: (%s) Name: %s}" (Token.string_of id.token) id.name
  ;;
  let print stmt = 
    Printf.sprintf "%s" stmt.name
  ;;
end

module Integer = struct
  type t = {
    token : Token.t;
    value : int;
  }
  [@@deriving show]

  let of_token (token: Token.t) = 
    let value = int_of_string token.literal in
    {token; value }
  ;;

  let eq a b = 
    a.value = b.value
  ;;

  let compare a b =
    compare a.value b.value
  ;;

  let string_of i = 
    Printf.sprintf "Integer: {Token: (%s) Value: %s}" (Token.string_of i.token) (string_of_int i.value)
  ;;

  let print stmt = 
    Printf.sprintf "%d" stmt.value
end

module rec Expression : sig
  type t = 
    | Boolean of Boolean.t
    | Identifier of Identifier.t
    | Integer of Integer.t
    | Prefix of Prefix.t
    | Infix of Infix.t
    | If of If.t
    | Function of Function.t
  [@@deriving show]

    val init : Token.t -> t
    val init_integer : Token.t -> t
    val eq : t -> t -> bool
    val string_of : t -> string
    val print : t -> string
    val init_prefix : Token.t -> t -> t;;
    val init_infix : Token.t -> t -> t -> t;;
    (* val init_if : Token.t -> t -> BlockStmt.t -> BlockStmt.t -> t;; *)
    val init_if : Token.t -> t -> BlockStmt.t -> t;;
    val init_if_else : Token.t -> t -> BlockStmt.t -> BlockStmt.t -> t;;
    val init_function : Token.t -> Identifier.t list -> BlockStmt.t -> t;;

end = struct 
  type t = 
    | Boolean of Boolean.t
    | Identifier of Identifier.t
    | Integer of Integer.t
    | Prefix of Prefix.t
    | Infix of Infix.t
    | If of If.t
    | Function of Function.t
  [@@deriving show]

  let init (token : Token.t) = 
    match token.t_type with 
      | Token.Ident -> Identifier (Identifier.of_token token)
      | Token.Int -> Integer (Integer.of_token token)
      | Token.False
      | Token.True -> Boolean (Boolean.of_token token)
      | Token.Bang -> failwith "Should not be here"
      | Token.Minus -> failwith "Should not be here"
      | _ -> failwith ("cannot init Expression with token" ^ Token.string_of token)
    (* Identifier (Identifier.of_token id) *)
  ;;

  let init_prefix (token : Token.t) (expr: t) : t  = 
    match token.t_type with 
      | Token.Bang -> Prefix (Prefix.init token expr)
      | Token.Minus -> Prefix (Prefix.init token expr)
      | _ -> failwith ("cannot init PrefixExpression with token" ^ Token.string_of token)
    (* Identifier (Identifier.of_token id) *)
  ;;

  let init_function (token : Token.t) (params: Identifier.t list) (body : BlockStmt.t) : t =
    match token.t_type with 
      | Token.Function -> Function (Function.init token params body)
      | _ -> failwith ("cannot init FunctionExpression with token" ^ Token.string_of token)


  let init_infix (token : Token.t) (left: t) (right : t) : t =
    match token.t_type with 
      | Token.Minus
      | Token.Plus
      | Token.Asterisk
      | Token.LT
      | Token.GT
      | Token.Eq
      | Token.NotEq
      | Token.Slash -> Infix (Infix.init token left right)
      | _ -> failwith ("cannot init InfixExpression with token" ^ Token.string_of token)
    (* Identifier (Identifier.of_token id) *)
  ;;

  let init_if_else (token : Token.t) (condition: t) (consequence : BlockStmt.t) (alternative : BlockStmt.t) : t =
    match token.t_type with 
      | Token.If -> If (If.init_else token condition consequence alternative)
      | _ -> failwith ("cannot init InfixExpression with token" ^ Token.string_of token)
  ;;
  let init_if (token : Token.t) (condition: t) (consequence : BlockStmt.t) : t =
    match token.t_type with 
      (* | Token.If -> If (If.init token condition consequence right) *)
      | Token.If -> If (If.init token condition consequence)
      | _ -> failwith ("cannot init InfixExpression with token" ^ Token.string_of token)
    (* Identifier (Identifier.of_token id) *)

  ;;

  let string_of expr = match expr with
    | Boolean expr -> "Expression.Boolean: " ^ Boolean.string_of expr
    | Identifier expr -> "Expression.Identifier: " ^ Identifier.string_of expr
    | Integer expr -> "Expression.Integer: " ^ Integer.string_of expr
    | Prefix expr -> "Expression.Prefix: " ^ Prefix.string_of expr
    | Infix expr -> "Expression.Infix: " ^ Infix.string_of expr
    | If expr -> "Expression.If: " ^ If.string_of expr
    | Function expr -> "Expression.Function: " ^ Function.string_of expr
  ;;

  let print expr = match expr with
    | Boolean expr -> Boolean.print expr
    | Identifier expr -> Identifier.print expr
    | Integer expr -> Integer.print expr
    | Prefix expr -> Prefix.print expr
    | Infix expr -> Infix.print expr
    | If expr -> If.print expr
    | Function expr -> Function.string_of expr
  ;;

  let init_integer i = 
    Integer (Integer.of_token i)
  ;;

  let eq a b = 
    (* let () = Printf.printf "EQAUSL{???: %s %s\n" (string_of a) (string_of b) in *)
    match (a, b) with 
      | (Boolean a, Boolean b) -> Boolean.eq a b
      | (Identifier a, Identifier b) -> Identifier.eq a b
      | (Integer a, Integer b) -> Integer.eq a b
      | (Prefix a, Prefix b) -> Prefix.eq a b
      | (Infix a, Infix b) -> Infix.eq a b
      | (If a, If b) -> If.eq a b
      | (Function a, Function b) -> Function.eq a b
      | _ -> false
    (* a = b *)
  ;;
end
and Prefix : sig
  type t = {
    token : Token.t;
    operator : string;
    right : Expression.t;
  }
  val init : Token.t -> Expression.t -> t
  val string_of : t -> string
  val print : t -> string
  val eq : t -> t -> bool
end = struct 
  type t = {
    token : Token.t;
    operator : string;
    right : Expression.t;
  }

  let init token expr = 
    {token; operator = token.literal; right = expr}

  let eq a b = 
    let is_token_eq = Token.eq a.token b.token in
    Printf.printf "token: %s == %s: %b" (Token.string_of a.token) (Token.string_of b.token) is_token_eq;
    let is_operator_eq = a.operator = b.operator in
    Printf.printf "operator: %s == %s: %b" a.operator b.operator is_operator_eq;
    let is_right_eq = Expression.eq a.right b.right in
    Printf.printf "right: %s == %s: %b" (Expression.string_of a.right) (Expression.string_of b.right) is_right_eq;

    is_token_eq && is_operator_eq && is_right_eq
  ;;

  let string_of expr = 
    Printf.sprintf "(%s %s %s)" (Token.string_of expr.token) expr.operator (Expression.string_of expr.right)
  ;;

  let print expr = 
    Printf.sprintf "(\'%s\' %s)" expr.operator (Expression.print expr.right)

end 
and Infix : sig
  type t = {
    token : Token.t;
    left : Expression.t;
    operator : string;
    right : Expression.t;
  }
  val init : Token.t -> Expression.t -> Expression.t -> t
  val string_of : t -> string
  val print : t -> string
  val eq : t -> t -> bool
end = struct 
  type t = {
    token : Token.t;
    left : Expression.t;
    operator : string;
    right : Expression.t;
  }

  let init token left right = 
    {token; left; operator = token.literal; right;}

  let string_of expr = 
    Printf.sprintf "InfixStmt: (%s <-> operator=\'%s\' <-> %s)" (Expression.string_of expr.left) expr.operator (Expression.string_of expr.right)


  let print expr = 
    Printf.sprintf "(%s \'%s\' %s)" (Expression.print expr.left) expr.operator (Expression.print expr.right)

  let eq a b = 
    let is_left_eq = Expression.eq a.left b.left in
    let is_operator_eq = a.operator = b.operator in
    let is_right_eq = Expression.eq a.right b.right in

    is_left_eq && is_operator_eq && is_right_eq

end
and Function : sig
  type t = {
    token : Token.t;
    parameters : Identifier.t list;
    body : BlockStmt.t;
  };;
  val string_of : t -> string
  val eq : t -> t -> bool
  val init : Token.t -> Identifier.t list -> BlockStmt.t -> t
end = struct 
  type t = {
    token : Token.t;
    parameters : Identifier.t list;
    body : BlockStmt.t;
  };;
  let init token parameters body = 
    {token; parameters; body;}
  ;;
  let string_of expr = 
    Printf.sprintf "%s ( ) { %s }" (Token.string_of expr.token) (BlockStmt.string_of expr.body)

  let eq a b = 
    let is_token_eq = Token.eq a.token b.token in
    let is_parameters_eq = List.for_all2 Identifier.eq a.parameters b.parameters in
    let is_body_eq = BlockStmt.eq a.body b.body in
    is_token_eq && is_parameters_eq && is_body_eq
  ;;

end
and If : sig
  type t = {
    token : Token.t;
    condition : Expression.t;
    consequence : BlockStmt.t;
    alternative : BlockStmt.t option;
  };;
  val string_of : t -> string
  val print : t -> string
  val eq : t -> t -> bool
  val init : Token.t -> Expression.t -> BlockStmt.t -> t
  val init_else : Token.t -> Expression.t -> BlockStmt.t -> BlockStmt.t -> t

end = struct
  type t = {
    token : Token.t;
    condition : Expression.t;
    consequence : BlockStmt.t;
    alternative : BlockStmt.t option;
  };;
  let print stmt =
    (* Printf.sprintf "if %s then %s else %s)" (Expression.print stmt.condition) (BlockStmt.string_of stmt.consequence) (BlockStmt.string_of stmt.alternative) *)
    Printf.sprintf "if %s then %s " (Expression.print stmt.condition) (BlockStmt.string_of stmt.consequence)
  ;;
  let string_of stmt = print stmt;;

  (* let init token condition consequence alternative =  *)
    (* {token; condition; consequence; alternative} *)
  let init token condition consequence = 
    {token; condition; consequence; alternative = None;}

  let init_else token condition consequence (alternative : BlockStmt.t) = 
    {token; condition; consequence; alternative = Some alternative; }

  let eq a b = 
    (* SHOULD I REMOVE TOKEN CHECK? *)
    let is_token_eq = Token.eq a.token b.token in
    Printf.printf "Token is equal:  %b\n" is_token_eq;
    let is_condition_eq = Expression.eq a.condition b.condition in
    Printf.printf "condition is equal:  %b\n" is_condition_eq;
    let is_consequence_eq = BlockStmt.eq a.consequence b.consequence in
    Printf.printf "consequence a:  %s\n" (BlockStmt.string_of a.consequence);
    Printf.printf "consequence b:  %s\n" (BlockStmt.string_of b.consequence);
    Printf.printf "consequence is equal:  %b\n" is_consequence_eq;
    (* let is_alternative_eq = BlockStmt.eq a.alternative b.alternative in *)

    (* is_token_eq && is_condition_eq && is_consequence_eq && is_alternative_eq *)
    is_token_eq && is_condition_eq && is_consequence_eq
  ;;
end
and Stmt : sig
  type t = 
      | Let of LetStmt.t
      | Return of ReturnStmt.t
      | Expression of ExpressionStmt.t
  [@@deriving show]
  val init : Token.t -> Token.t -> Expression.t -> t
  (* val init_integer : Token.t -> t *)
  val eq : t -> t -> bool
  val string_of : t -> string
  val _print : t -> string
end = struct
  type t = 
      | Let of LetStmt.t
      | Return of ReturnStmt.t
      | Expression of ExpressionStmt.t
  [@@deriving show]
    
  let _string_of = function
    | Let stmt -> LetStmt.string_of stmt
    | Return stmt -> ReturnStmt.string_of stmt
    | Expression stmt -> ExpressionStmt.string_of stmt
  ;;

  let init (stmt_token : Token.t) (aux_token : Token.t) (expr : Expression.t) =
    match (stmt_token.t_type) with
    | (Token.Let) -> 
        let id = Identifier.of_token aux_token in 
        Let (LetStmt.init stmt_token id expr)

    | (Token.Return) -> Return (ReturnStmt.init stmt_token expr)
    (* | (Token.Bang) -> failwith "unimplemented!" *)
    (* | (Token.Minus) -> failwith "unimplemented!" *)
    | _ -> failwith "invalid stmt type"
  ;;

  let string_of = function
    | Let stmt -> "Stmt.Let: " ^ LetStmt.string_of stmt
    | Return stmt -> "Stmt.Return: " ^ ReturnStmt.string_of stmt
    | Expression stmt -> "Stmt.Expression: " ^ ExpressionStmt.string_of stmt
  ;;

  let _print = function
    | Let stmt -> LetStmt.string_of stmt
    | Return stmt -> ReturnStmt.string_of stmt
    | Expression stmt -> ExpressionStmt.print stmt
  ;;

  let eq (a : t) (b : t) : bool = 
    match (a, b) with
    | (Let a, Let b) -> LetStmt.eq a b
    | (Return a, Return b) -> ReturnStmt.eq a b
    | (Expression a, Expression b) -> ExpressionStmt.eq a b
    | (a, b) -> ("Cannot compare statements\n" ^ string_of a ^ "\n" ^ (string_of b) |> failwith)


  (* let print stmt =  *)
  (*   print_endline (string_of stmt) *)
  (* ;; *)
end
and ExpressionStmt : sig
  type t = {
    token : Token.t;
    expr : Expression.t;
  }
  [@@deriving show]
  val init : Token.t -> Expression.t -> t
  val eq : t -> t -> bool
  val string_of : t -> string
  val print : t -> string

end = struct
  type t = {
    token : Token.t;
    expr : Expression.t;
  }
  [@@deriving show]

  (* let token_literal expr = expr.token.literal;; *)

  let init (token : Token.t) (expr : Expression.t) = 
    {token; expr }
  ;;

  let eq a b = 
    match (a, b) with
    | ({token = a_token; expr = a_expr}, {token = b_token; expr = b_expr}) -> 
      (Token.eq a_token b_token) && (Expression.eq a_expr b_expr)
  ;;
  let string_of (stmt : t) = 
    Printf.sprintf "ExpressionStmt: {(%s) (%s)}" (Token.string_of stmt.token) (Expression.string_of stmt.expr)
  ;;
  let print (stmt : t) = 
    Printf.sprintf "%s" (Expression.print stmt.expr)
  ;;

end
and LetStmt : sig
  type t = {
    token : Token.t;
    id : Identifier.t;
    value : Expression.t;
  }
  val init : Token.t -> Identifier.t -> Expression.t -> t
  val eq : t -> t -> bool
  val string_of : t -> string

end = struct
  type t = {
    token : Token.t;
    id : Identifier.t;
    value : Expression.t;
  }
    [@@deriving show];;

  let init token id expr = 
    {token; id; value = expr}
  ;;

  let string_of (stmt : t) = 
    Printf.sprintf "%s %s = %s;" stmt.token.literal stmt.id.token.literal (Expression.string_of stmt.value)
  ;;

  let eq (a : t) (b : t) = 
    match (a, b) with
    | ({token = a_token; id = _a_id; value = _a_value}, {token = b_token; id = _b_id; value = _b_value}) -> 
      (Token.eq a_token b_token)  &&
      (Identifier.eq _a_id _b_id) &&
      (Expression.eq _a_value _b_value)
  ;;
end

and ReturnStmt : sig
  type t = {
    token : Token.t;
    expr : Expression.t;
    (* value : expr; *)
  }
    [@@deriving show];;
  val init : Token.t -> Expression.t -> t
  val eq : t -> t -> bool
  val string_of : t -> string

end = struct
  type t = {
    token : Token.t;
    expr : Expression.t;
    (* value : expr; *)
  }
    [@@deriving show];;

  (* let token_literal = "return";; *)
  (* let t_type = Token.Return;; *)
  (* let tok: Token.t = {t_type = t_type; literal = token_literal};; *)

  let init token expr = 
    {token; expr}
  ;;

  let eq a b = 
    match (a, b) with
    | ({token = a_token; expr = a_expr}, {token = b_token; expr = b_expr}) -> 
      (Token.eq a_token b_token) && (Expression.eq a_expr b_expr)
      (* (Token.eq a_token b_token)  *)
  ;;

  let string_of stmt = 
      Printf.sprintf "ReturnStmt: {Token: (%s) value: (%s)" (Token.string_of stmt.token) (Expression.string_of stmt.expr)

end
and BlockStmt : sig
  type t = {
    token: Token.t;
    statements: Stmt.t list;
  }
  [@@deriving show]
  val init : Token.t -> Stmt.t list -> t
  (* (* val init_integer : Token.t -> t *) *)
  val eq : t -> t -> bool
  val string_of : t -> string
  (* val _print : t -> string *)

end = struct
  type t = {
    token: Token.t;
    statements: Stmt.t list;
  }

  let init token statements = 
    { token; statements; } 
  ;;
  let rec block_eq a b = 
    match (a, b) with
    | ([], []) -> true
    | ([], _) -> false
    | (_, []) -> false
    | (a_stmt::a_stmts, b_stmt::b_stmts) -> 
        (Stmt.eq a_stmt b_stmt) && block_eq a_stmts b_stmts
  ;;

  let rec string_of_statements (stmts : Stmt.t list) = 
    match stmts with
    | [] -> ""
    | stmt::stmts -> 
      (Stmt.string_of stmt) ^ "\n" ^ (string_of_statements stmts)

  let string_of (block : t) = 
    Printf.sprintf "BlockStmt: {Token: (%s) statements: (%s)" (Token.string_of block.token) (string_of_statements block.statements)
  

  let eq a b = 
    match (a, b) with
    | ({token = a_token; statements = a_statements}, {token = b_token; statements = b_statements}) -> 
    let token_is_equal = (Token.eq a_token b_token) in
      (token_is_equal) && (block_eq  a_statements b_statements)
        (* (eq {token = a_token; statements = a_stmts} {token = b_token; statements = b_stmts}) *)


end

type program = Stmt.t list

(* let rec string_of program output = match program with   *)
(*   | [] -> "EOF\n" *)
(*   | hd :: tl ->  *)
(*       let stmt = Stmt.string_of hd in *)
(*       let output = Printf.sprintf "Stmt: %s\n" stmt in *)
(*       string_of tl output *)

let rec print_program = function 
  | [] -> print_endline "PrintProgram EOF\n"
  | hd :: tl -> 
    let stmt = Stmt._print hd in
    let () = print_endline ("PrintProgram Stmt: " ^ stmt) in
    print_program tl
