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

  let string_of id = 
    Printf.sprintf "Identifier: {Token: (%s) Name: %s}" (Token.string_of id.token) id.name
  ;;
  let print stmt = 
    print_endline (string_of stmt)
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
end
(* module Expr = struct  *)
(*   type t = Map.Make(String).t *)
(*   [@@deriving show] *)
(* end *)


module Expression = struct 
  type t = 
    | Identifier of Identifier.t
    | Integer of Integer.t
    | Unknown
  [@@deriving show]

  let init (token : Token.t) = 
    match token.t_type with 
      | Token.Ident -> Identifier (Identifier.of_token token)
      | Token.Int -> Integer (Integer.of_token token)
      | _ -> Unknown
    (* Identifier (Identifier.of_token id) *)
  ;;
  let string_of = function
    | Identifier id -> Identifier.string_of id
    | Integer i -> Integer.string_of i
    | Unknown -> "Unknown"
  ;;

  let init_integer i = 
    Integer (Integer.of_token i)
  ;;

  let eq a b = 
    (* let () = Printf.printf "EQAUSL{???: %s %s\n" (string_of a) (string_of b) in *)
    match (a, b) with 
      | (Identifier a, Identifier b) -> Identifier.eq a b
      | (Integer a, Integer b) -> Integer.eq a b
      | (Unknown, Unknown) -> true
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
  let string_of stmt = 
    Printf.sprintf "ExpressionStmt: {(%s) (%s)}" (Token.string_of stmt.token) (Expression.string_of stmt.expr)
  ;;

end

module LetStmt = struct
  type t = {
    token : Token.t;
    id : Identifier.t;
    value : Expression.t;
  }
    [@@deriving show];;

  (* let token_literal = "let" *)
  (* let t_type = Token.Let;; *)
  (* let token : Token.t = {t_type = t_type; literal = token_literal};; *)

  let init token id expr = 
    {token; id; value = expr}
  ;;

  let string_of (stmt : t) = 
    Printf.sprintf "LetStmt: %s %s = (%s);" stmt.token.literal stmt.id.token.literal (Expression.string_of stmt.value)
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

  let init (stmt_token : Token.t) (id : Identifier.t) (expr : Expression.t) = 
    match (stmt_token.t_type, expr) with
    | (Token.Let, expr) -> (
      match expr with 
        | Expression.Identifier _ -> Let (LetStmt.init stmt_token id expr)
        | Expression.Integer _ -> Let (LetStmt.init stmt_token id expr)
        (* | Expression.Unknown -> Let (LetStmt.init stmt_token id Expression.Unknown) *)
        | Expression.Unknown -> failwith "Cannot initiate let statement with unknown expression"
    )

    | (Token.Return, expr) -> (
      match expr with 
        | Expression.Identifier _ -> Return (ReturnStmt.init stmt_token expr)
        | Expression.Integer _ -> Return (ReturnStmt.init stmt_token expr)
        (* | Expression.Unknown -> Return (ReturnStmt.init stmt_token Expression.Unknown) *)
        | Expression.Unknown -> failwith "Cannot initiate return statement with unknown expression"
    )
(* Return (ReturnStmt.init id.token Expression.Unknown) *)
    | _ -> failwith "invalid stmt type"
  ;;

  let string_of = function
    | Let stmt -> LetStmt.string_of stmt
    | Return stmt -> ReturnStmt.string_of stmt
    | Expression stmt -> ExpressionStmt.string_of stmt
    (* | _ -> failwith "Cannot convert stmt to string" *)

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
  | [] -> print_endline "EOF\n"
  | hd :: tl -> 
    let stmt = Stmt.string_of hd in
    let () = print_endline ("Stmt: " ^ stmt) in
    print_program tl
