module Token = Token.Token
module Lexer = Lex.Lexer
module Boolean = Ast.Boolean
module Identifier = Ast.Identifier
module Integer = Ast.Integer
module Expression = Ast.Expression
module Prefix = Ast.Prefix
module Infix = Ast.Infix
module If = Ast.If
module Stmt = Ast.Stmt
module ExpressionStmt = Ast.ExpressionStmt
module LetStmt = Ast.LetStmt
module ReturnStmt = Ast.ReturnStmt
module BlockStmt = Ast.BlockStmt
type program = Stmt.t list
val print_program : Stmt.t list -> unit
module Parser :
  sig
    type t = {
      lexer : Lexer.t;
      cur_token : Token.t;
      peek_token : Token.t;
    }
    val cur_token_is : t -> Token.t_type -> bool
    val peek_token_is : t -> Token.t_type -> bool
    val next_token : t -> t
    val parse_integer : t -> t * Expression.t
    val parse_boolean : t -> t * Expression.t
    val parse_identifier : t -> t * Expression.t
    val init : string -> t
    val next_token_if : t -> Token.t_type -> t
    val cur_precedence : t -> Token.precedence
    val peek_precedence : t -> Token.precedence
    val parse_expr : t -> Token.precedence -> t * Expression.t
    val infix_loop :
      t -> Token.precedence -> Expression.t -> t * Expression.t
    val parse_grouped_expr : t -> t * Expression.t
    val parse_prefix_expr : t -> t * Expression.t
    val parse_block_loop : t -> Token.t -> Stmt.t list -> t * BlockStmt.t
    val parse_block_statement : t -> t * BlockStmt.t
    val parse_if_expr : t -> t * Expression.t
    val prefix : t -> t * Expression.t
    val parse_infix_expr : t -> Expression.t -> t * Expression.t
    val infix : t -> Expression.t -> t * Expression.t
    val parse_expr_stmt : t -> t * Stmt.t
    val skip_until : t -> Token.t_type -> t
    val parse_let_stmt : t -> t * Stmt.t
    val parse_return_stmt : t -> t * Stmt.t
    val parse_stmt : t -> t * Stmt.t
    val parse_program : t -> Stmt.t list -> Stmt.t list
  end

