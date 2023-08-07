include Token


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

module rec Prefix : sig
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

end and Infix : sig
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
    print_endline "-----------------------";
    print_endline "Starting infix init";
    print_endline (Token.string_of token);
    print_endline (Expression.string_of left);
    print_endline (Expression.string_of right);
    print_endline "END";
    print_endline "-----------------------";
    {token; left; operator = token.literal; right;}
    (* {left; operator = token.literal; right;} *)

  let string_of expr = 
    Printf.sprintf "InfixStmt: (%s <-> operator=\'%s\' <-> %s)" (Expression.string_of expr.left) expr.operator (Expression.string_of expr.right)

  let print expr = 
    Printf.sprintf "(%s \'%s\' %s)" (Expression.print expr.left) expr.operator (Expression.print expr.right)

  let eq a b = 
    (* let is_token_eq = Token.eq a.token b.token in *)
    (* Printf.printf "token: %s == %s: %b" (Token.string_of a.token) (Token.string_of b.token) is_token_eq; *)
    let is_left_eq = Expression.eq a.left b.left in
    Printf.printf "left: %s == %s: %b" (Expression.string_of a.left) (Expression.string_of b.left) is_left_eq;
    let is_operator_eq = a.operator = b.operator in
    Printf.printf "operator: %s == %s: %b" a.operator b.operator is_operator_eq;
    let is_right_eq = Expression.eq a.right b.right in
    Printf.printf "right: %s == %s: %b" (Expression.string_of a.right) (Expression.string_of b.right) is_right_eq;

    is_left_eq && is_operator_eq && is_right_eq
    (* is_token_eq && is_left_eq && is_operator_eq && is_right_eq *)

end
and Expression : sig
  type t = 
    | Identifier of Identifier.t
    | Integer of Integer.t
    | Prefix of Prefix.t
    | Infix of Infix.t
  [@@deriving show]

    val init : Token.t -> t
    val init_integer : Token.t -> t
    val eq : t -> t -> bool
    val string_of : t -> string
    val print : t -> string
    val init_prefix : Token.t -> t -> t;;
    val init_infix : Token.t -> t -> t -> t;;
end = struct 
  type t = 
    | Identifier of Identifier.t
    | Integer of Integer.t
    | Prefix of Prefix.t
    | Infix of Infix.t
  [@@deriving show]

  let init (token : Token.t) = 
    match token.t_type with 
      | Token.Ident -> Identifier (Identifier.of_token token)
      | Token.Int -> Integer (Integer.of_token token)
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

  let string_of expr = match expr with
    | Identifier expr -> "Expression.Identifier: " ^ Identifier.string_of expr
    | Integer expr -> "Expression.Integer: " ^ Integer.string_of expr
    | Prefix expr -> "Expression.Prefix: " ^ Prefix.string_of expr
    | Infix expr -> "Expression.Infix: " ^ Infix.string_of expr
  ;;

  let print expr = match expr with
    | Identifier expr -> Identifier.print expr
    | Integer expr -> Integer.print expr
    | Prefix expr -> Prefix.print expr
    | Infix expr -> Infix.print expr
  ;;

  let init_integer i = 
    Integer (Integer.of_token i)
  ;;

  let eq a b = 
    (* let () = Printf.printf "EQAUSL{???: %s %s\n" (string_of a) (string_of b) in *)
    match (a, b) with 
      | (Identifier a, Identifier b) -> Identifier.eq a b
      | (Integer a, Integer b) -> Integer.eq a b
      | (Prefix a, Prefix b) -> Prefix.eq a b
      | (Infix a, Infix b) -> Infix.eq a b
      | _ -> false
    (* a = b *)
  ;;
end

module ExpressionStmt = struct
  type t = {
    token : Token.t;
    expr : Expression.t;
  }
  [@@deriving show]

  let token_literal expr = expr.token.literal;;

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

module LetStmt = struct
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
    let () = print_endline "-------------------------" in
    let () = print_endline "A: " in 
    let () = print_endline (string_of a) in 
    let () = print_endline "B: " in 
    let () = print_endline (string_of b) in 
    let () = print_endline "-------------------------" in
    let () = print_endline ("a.token: " ^ (Token.string_of a.token)) in 
    let () = print_endline ("b.token: " ^ (Token.string_of b.token)) in 
    let token_eqs = (Token.eq a.token b.token) in
    let id_eqs = (Identifier.eq a.id b.id) in
    let expr_eqs = (Expression.eq a.value b.value) in
    let () = print_endline ("token_eqs: " ^ (string_of_bool token_eqs)) in 
    let () = print_endline ("id_eqs: " ^ (string_of_bool id_eqs)) in 
    let () = print_endline ("expr_eqs: " ^ (string_of_bool expr_eqs)) in 

    match (a, b) with
    | ({token = a_token; id = _a_id; value = _a_value}, {token = b_token; id = _b_id; value = _b_value}) -> 
      (Token.eq a_token b_token)  &&
      (Identifier.eq _a_id _b_id) &&
      (Expression.eq _a_value _b_value)
      (* && (Identifier.eq a_id b_id) && (Expression.eq a_value b_value) *)
      (* (Token.eq a_token b_token) && (Identifier.eq a_id b_id) *)
  
  ;;

  let print stmt = 
    print_endline (string_of stmt)
  ;;

end

module ReturnStmt = struct
  type t = {
    token : Token.t;
    expr : Expression.t;
    (* value : expr; *)
  }
    [@@deriving show];;

  let token_literal = "return";;
  let t_type = Token.Return;;
  let tok: Token.t = {t_type = t_type; literal = token_literal};;

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

module Stmt = struct
  type t = 
      | Let of LetStmt.t
      | Return of ReturnStmt.t
      | Expression of ExpressionStmt.t
  [@@deriving show]
    
  let token_literal _ = "";;


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
    | (Token.Bang) -> failwith "unimplemented!"
    | (Token.Minus) -> failwith "unimplemented!"
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


  let print stmt = 
    print_endline (string_of stmt)
  ;;
end

type program = Stmt.t list

let rec print_program = function 
  | [] -> print_endline "PrintProgram EOF\n"
  | hd :: tl -> 
    let stmt = Stmt._print hd in
    let () = print_endline ("PrintProgram Stmt: " ^ stmt) in
    print_program tl
