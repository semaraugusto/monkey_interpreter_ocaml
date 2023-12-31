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
  let pp b = "(Bool: " ^ string_of_bool b.value ^ ")";;

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
  let pp id = "(Identifier: " ^ id.name ^ ")";;

  let rec string_of_arguments = function
    | [] -> ""
    | (hd :: tl) -> 
        (let hd_string = string_of hd in
        let tl_string = string_of_arguments tl in
        let res = if tl_string = "" then 
          hd_string 
        else 
          hd_string ^ ", " ^ tl_string 
        in
        res)
  ;;

  let string_of_complete id = 
    Printf.sprintf "Identifier: {Token: (%s) Name: %s}" (Token.string_of id.token) id.name
  ;;
  let print stmt = 
    Printf.sprintf "%s" stmt.name
  ;;
end
(* type arguments = Identifier.t list *)


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
    Printf.sprintf "INT: {Token: (%s) Value: %s}" (Token.string_of i.token) (string_of_int i.value)
  ;;

  let pp i = "(INT: " ^ string_of_int i.value ^ ")";;

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
    | Call of Call.t
  [@@deriving show]

    val init : Token.t -> t
    val init_integer : Token.t -> t
    val eq : t -> t -> bool
    val string_of : t -> string
    val pp : t -> string
    val string_of_arguments : t list -> string
    val print : t -> string
    val init_prefix : Token.t -> t -> t;;
    val init_infix : Token.t -> t -> t -> t;;
    (* val init_if : Token.t -> t -> BlockStmt.t -> BlockStmt.t -> t;; *)
    val init_if : Token.t -> t -> BlockStmt.t -> t;;
    val init_if_else : Token.t -> t -> BlockStmt.t -> BlockStmt.t -> t;;
    val init_function : Token.t -> Identifier.t list -> BlockStmt.t -> t;;
    val init_call : Token.t -> Expression.t -> Expression.t list -> t;;

end = struct 
  type t = 
    | Boolean of Boolean.t
    | Identifier of Identifier.t
    | Integer of Integer.t
    | Prefix of Prefix.t
    | Infix of Infix.t
    | If of If.t
    | Function of Function.t
    | Call of Call.t
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

  let init_call (token : Token.t) (fn: t) (arguments : t list): t =
    match token.t_type with 
      | Token.LParen -> Call (
      Call.init token fn arguments)
      | _ -> failwith ("cannot init CallExpression with token" ^ Token.string_of token)
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
    | Call expr -> "Expression.Call: " ^ Call.string_of expr
  ;;
    
  let pp expr = match expr with
    | Boolean expr -> Boolean.pp expr
    | Identifier expr -> Identifier.pp expr
    | Integer expr -> Integer.pp expr
    | Prefix expr -> Prefix.pp expr
    | Infix expr -> Infix.pp expr
    | If expr -> If.pp expr
    | Function expr -> Function.string_of expr
    | Call expr -> Call.string_of expr
  ;;

  let rec string_of_arguments = function
    | [] -> ""
    | (hd :: tl) -> 
        (let hd_string = string_of hd in
        let tl_string = string_of_arguments tl in
        let res = if tl_string = "" then 
          hd_string 
        else 
          hd_string ^ ", " ^ tl_string 
        in
        res)
  ;;

  let print expr = match expr with
    | Boolean expr -> Boolean.print expr
    | Identifier expr -> Identifier.print expr
    | Integer expr -> Integer.print expr
    | Prefix expr -> Prefix.print expr
    | Infix expr -> Infix.print expr
    | If expr -> If.print expr
    | Function expr -> Function.string_of expr
    | Call expr -> Call.string_of expr
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
      | (Call a, Call b) -> Call.eq a b
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
  val pp : t -> string
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
    let is_operator_eq = a.operator = b.operator in
    let is_right_eq = Expression.eq a.right b.right in

    is_token_eq && is_operator_eq && is_right_eq
  ;;

  let string_of expr = 
    Printf.sprintf "(%s %s %s)" (Token.string_of expr.token) expr.operator (Expression.string_of expr.right)
  ;;

  let pp expr = 
    Printf.sprintf "prefix: (%s %s %s)" (Token.pp expr.token) expr.operator (Expression.pp expr.right)

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
  val pp : t -> string
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
    Printf.sprintf "(%s \'%s\' %s)" (Expression.string_of expr.left) expr.operator (Expression.string_of expr.right)


  let print expr = 
    Printf.sprintf "(%s \'%s\' %s)" (Expression.print expr.left) expr.operator (Expression.print expr.right)

  let pp expr = 
    Printf.sprintf "(%s \'%s\' %s)" (Expression.pp expr.left) expr.operator (Expression.pp expr.right)

  let eq a b = 
    let is_left_eq = Expression.eq a.left b.left in
    let is_operator_eq = a.operator = b.operator in
    let is_right_eq = Expression.eq a.right b.right in

    is_left_eq && is_operator_eq && is_right_eq

end
and Call : sig
  type t = {
    token : Token.t;
    fn : Expression.t;
    arguments : Expression.t list;
  };;
  val string_of : t -> string
  val pp : t -> string
  val eq : t -> t -> bool
  val init : Token.t -> Expression.t -> Expression.t list -> t
end = struct 
  type t = {
    token : Token.t;
    fn : Expression.t;
    arguments : Expression.t list;
  };;

  let init token fn arguments = 
    { token; fn; arguments; }

  let eq a b = 
    let is_token_eq = Token.eq a.token b.token in
    let is_fn_equal = Expression.eq a.fn b.fn in
    match ((List.length a.arguments) == (List.length b.arguments)) with
    true -> 
      let are_arguments_equal = List.for_all2 Expression.eq a.arguments b.arguments in
      is_token_eq && is_fn_equal && are_arguments_equal
    | false -> false
  ;;  

  let string_of expr = 
    Printf.sprintf "%s (%s)" (Expression.string_of expr.fn) (Expression.string_of_arguments expr.arguments)

  let pp expr = "(call: " ^ string_of expr ^ ")";;
  ;;

end
and Function : sig
  type t = {
    token : Token.t;
    parameters : Identifier.t list;
    body : BlockStmt.t;
  };;
  val string_of : t -> string
  val pp : t -> string
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
    Printf.sprintf "%s (%s) { %s }" (Token.string_of expr.token) (String.concat "," (List.map Identifier.string_of expr.parameters)) (BlockStmt.string_of expr.body)
  (* | BlockStatement block -> Printf.sprintf "Block: %s" (String.concat "\n" (List.map Stmt.string_of block.statements)) *)

  let pp expr = 
    Printf.sprintf "(Function: %s)" (string_of expr)
  ;;

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
  val pp : t -> string
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
    match stmt.alternative with
    | Some alternative -> 
      Printf.sprintf "IfExpr: (if %s then %s else %s)" (Expression.string_of stmt.condition) (BlockStmt.string_of stmt.consequence) (BlockStmt.string_of alternative)
    | None -> 
      Printf.sprintf "IfExpr (if %s then %s)" (Expression.string_of stmt.condition) (BlockStmt.string_of stmt.consequence)
  ;;
  let string_of stmt = print stmt;;

  let pp stmt =
    match stmt.alternative with
    | Some alternative -> 
      Printf.sprintf "(IF %s THEN\n %s ELSE\n %s)" (Expression.pp stmt.condition) (BlockStmt.pp stmt.consequence) (BlockStmt.pp alternative)
    | None -> 
      Printf.sprintf "(IF condition=%s THEN\nconsequence=%s)" (Expression.pp stmt.condition) (BlockStmt.pp stmt.consequence)
  ;;

  (* let init token condition consequence alternative =  *)
    (* {token; condition; consequence; alternative} *)
  let init token condition consequence = 
    {token; condition; consequence; alternative = None;}

  let init_else token condition consequence (alternative : BlockStmt.t) = 
    {token; condition; consequence; alternative = Some alternative; }

  let eq a b = 
    (* SHOULD I REMOVE TOKEN CHECK? *)
    let is_token_eq = Token.eq a.token b.token in
    let is_condition_eq = Expression.eq a.condition b.condition in
    let is_consequence_eq = BlockStmt.eq a.consequence b.consequence in
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
  val pp : t -> string
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

  let pp = function
    | Let stmt -> LetStmt.pp stmt
    | Return stmt -> ReturnStmt.pp stmt
    | Expression stmt -> ExpressionStmt.pp stmt
  ;;

  let eq (a : t) (b : t) : bool = 
    match (a, b) with
    | (Let a, Let b) -> LetStmt.eq a b
    | (Return a, Return b) -> ReturnStmt.eq a b
    | (Expression a, Expression b) -> ExpressionStmt.eq a b
    | (a, b) -> ("Cannot compare statements a=" ^ string_of a ^ "\nb=" ^ (string_of b) |> failwith)


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
  val pp : t -> string

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

  let pp (stmt : t) = 
    Printf.sprintf "%s" (Expression.pp stmt.expr)
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
  val pp : t -> string

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

  let pp (stmt : t) = 
    Printf.sprintf "LET %s %s = %s;" stmt.token.literal stmt.id.token.literal (Expression.pp stmt.value)
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
  val pp : t -> string

end = struct
  type t = {
    token : Token.t;
    expr : Expression.t;
    (* value : expr; *)
  }
    [@@deriving show];;

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

  let pp (stmt : t) = 
    Printf.sprintf "return %s;" (Expression.pp stmt.expr)
  ;;

end
and BlockStmt : sig
  type t = {
    statements: Stmt.t list;
  }
  [@@deriving show]
  val init : Stmt.t list -> t
  val eq : t -> t -> bool
  val string_of : t -> string
  val pp : t -> string
  val string_of_statements : Stmt.t list -> string

end = struct
  type t = {
    statements: Stmt.t list;
  }

  let init statements = 
    { statements; } 
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
      (Stmt.pp stmt) ^ "\n" ^ (string_of_statements stmts)

  let string_of (block : t) = 
    Printf.sprintf "BlockStmt: {statements: (%s)" (string_of_statements block.statements)

  let pp (block : t) = 
    Printf.sprintf "(Block: %s)" (String.concat "\n" (List.map Stmt.pp block.statements))
  
  

  let eq a b = 
    match (a, b) with
    | ({statements = a_statements}, {statements = b_statements}) -> 
     block_eq  a_statements b_statements
end

type program = Stmt.t list

module Node = struct
  type t = 
    | Program of program
    | Statement of Stmt.t
    | BlockStatement of BlockStmt.t
    | Expression of Expression.t
  ;;

  let of_stmt stmt = Statement stmt;;
  let of_program prog = Program prog;;
  let of_expression expr = Expression expr;;

  let string_of = function 
  | Program prog -> Printf.sprintf "Program: \n-------------------------------------------------\n%s\n-------------------------------------------------\n" (String.concat "\n" (List.map Stmt.string_of prog))
  | Statement stmt -> Printf.sprintf "Statement: %s" (Stmt.string_of stmt)
  | BlockStatement block -> Printf.sprintf "Block: %s" (String.concat "\n" (List.map Stmt.string_of block.statements))
  | Expression expr -> Printf.sprintf "Expression: %s" (Expression.string_of expr)
  ;;

  let pp = function 
  | Program prog -> "(Program: " ^ String.concat "\n" (List.map Stmt.pp prog) ^ ")"
  | Statement stmt -> Stmt.pp stmt
  | BlockStatement block -> String.concat "\n" (List.map Stmt.pp block.statements)
  | Expression expr -> Expression.pp expr

end

let rec print_program = function 
  | [] -> print_endline "PrintProgram EOF\n"
  | hd :: tl -> 
    let stmt = Stmt.pp hd in
    let () = print_endline ("PrintProgram Stmt: " ^ stmt) in
    print_program tl

let rec show_program = function 
  | [] -> print_endline "PrintProgram EOF\n"
  | hd :: tl -> 
    let stmt = Node.pp hd in
    let () = print_endline ("PrintProgram Stmt: " ^ stmt) in
    show_program tl
