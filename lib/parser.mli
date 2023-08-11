module Token = Token.Token
module Lexer = Lex.Lexer
module Boolean = Ast.Boolean
module Identifier = Ast.Identifier
module Integer = Ast.Integer
module Expression = Ast.Expression
module Prefix = Ast.Prefix
module Infix = Ast.Infix
module Function = Ast.Function
module If = Ast.If
module Stmt = Ast.Stmt
module ExpressionStmt = Ast.ExpressionStmt
module LetStmt = Ast.LetStmt
module ReturnStmt = Ast.ReturnStmt
module BlockStmt = Ast.BlockStmt
type program = Stmt.t list
val print_program : Stmt.t list -> unit
module ParseError : sig 
  type t =
    | NoPrefixParseFn of string
    | NoInfixParseFn of string
    | ExpectedSemicolon of string
    | ExpectedIdentifier of string
    | ExpectedInteger of string
    | ExpectedBoolean of string

  val string_of : t -> string
end

module Parser :
  sig
    type t = {
      lexer : Lexer.t;
      cur_token : Token.t;
      peek_token : Token.t;
    }
    val init : string -> t
    val next_token_if : t -> Token.t_type -> t
    val parse_parameters : t -> Identifier.t list -> t * Identifier.t list
    val parse_function_parameters : t -> t * Identifier.t list
    val cur_precedence : t -> Token.precedence
    val peek_precedence : t -> Token.precedence
    val parse_expr : t -> Token.precedence -> (t * Expression.t, ParseError.t) result
    val parse_let_stmt : t -> (t * Stmt.t, ParseError.t) result
    val parse_return_stmt : t -> (t * Stmt.t, ParseError.t) result
    val parse_stmt : t -> (t * Stmt.t, ParseError.t) result
    val infix_loop :
      t -> Token.precedence -> Expression.t -> (t * Expression.t, ParseError.t) result
    val parse_grouped_expr : t -> (t * Expression.t, ParseError.t) result
    val parse_prefix_expr : t -> (t * Expression.t, ParseError.t) result
    val parse_block_loop : t -> Token.t -> Stmt.t list -> (t * BlockStmt.t, ParseError.t) result
    val parse_block_stmt : t -> (t * BlockStmt.t, ParseError.t) result
    val parse_function_literal : t -> (t * Expression.t, ParseError.t) result
    val parse_if_expr : t -> (t * Expression.t, ParseError.t) result
    val parse_expr_stmt : t -> (t * Stmt.t, ParseError.t) result
    val prefix : t -> (t * Expression.t, ParseError.t) result
    val parse_infix_expr : t -> Expression.t -> (t * Expression.t, ParseError.t) result
    val infix : t -> Expression.t -> (t * Expression.t, ParseError.t) result
    val skip_until : t -> Token.t_type -> t
    val parse_program : t -> Stmt.t list -> (Stmt.t list, ParseError.t) result
  end

