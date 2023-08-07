include Token
include Lex
include Ast

(* module PrefixParseFns = Map.Make(String);; *)
module PrefixParseFns = Map.Make(struct type t = Token.t_type let compare = Token.compare end)

module Parser = struct 
  type t = {
    lexer : Lexer.t;
    cur_token : Token.t;
    peek_token : Token.t;
    (* prefix_fns: (t -> (t * Expression.t))  PrefixParseFns.t; *)
  } [@@deriving show];;

  let cur_token_is parser t_type = 
    parser.cur_token.t_type = t_type
  ;;

  let peek_token_is parser t_type = 
    parser.peek_token.t_type = t_type
  ;;

  let next_token parser = 
    let lexer = parser.lexer in
    let cur_token = parser.peek_token in
    let peek_token = Lexer.next_token parser.lexer in
    { 
      lexer; 
      cur_token; 
      peek_token; 
      (* prefix_fns = parser.prefix_fns  *)
    }
  ;;
  
  let parse_integer (parser : t) : (t * Ast.Expression.t) =
    assert (cur_token_is parser Token.Int);
    let token = parser.cur_token in
    (parser, (Ast.Expression.init token))
  ;;

  let parse_boolean (parser : t) : (t * Ast.Expression.t) =
    assert ((cur_token_is parser Token.True) || (cur_token_is parser Token.False));
    let token = parser.cur_token in
    (parser, (Ast.Expression.init token))
  ;;

  let parse_identifier (parser : t) : (t * Ast.Expression.t) =
    assert (cur_token_is parser Token.Ident);
    let token = parser.cur_token in
    (parser, Ast.Expression.init token)
  ;;

  let init code = 
    let lexer = Lexer.init code in
    let cur_token = Lexer.next_token lexer in
    let peek_token = Lexer.next_token lexer in

    { 
      lexer; 
      cur_token; 
      peek_token; 
      (* prefix_fns  *)
    }
  let next_token_if parser (t_type : Token.t_type) = match (peek_token_is parser t_type) with
    | true -> next_token parser
    (* | false -> failwith ("failed to parse let statement") *)
    | false -> failwith ("failed to parse statement. Expected: " ^ (Token.string_of_t_type t_type) ^ " got: " ^ Token.string_of_t_type parser.peek_token.t_type)
  ;;

  let cur_precedence (parser : t) = 
    Token.precedence_of parser.cur_token
  ;;
  let peek_precedence (parser : t) = 
    Token.precedence_of parser.peek_token
  ;;

  let rec parse_expr (parser : t) (precedence: Token.precedence) :  (t * Expression.t) = 
    let (parser, left) = prefix parser in
    Printf.printf "parse_expr : %s left: %s\n" (Token.string_of parser.cur_token) (Expression.print left);

    infix_loop parser precedence left

  and
  infix_loop (parser : t) (precedence : Token.precedence) (left: Expression.t) : (t * Expression.t) = 
    Printf.printf "infix-loop : %s\nleft: %s\n" (Token.string_of parser.cur_token) (Expression.print left);
    Printf.printf "not peek_is_semicolon : %b\n" (not (peek_token_is parser Token.Semicolon));
    Printf.printf "precedence : %s\n" (Token.string_of_precedence precedence);
    Printf.printf "peek_precedence : %s\n" (Token.string_of_precedence (peek_precedence parser));
    Printf.printf "precedence < peek_precedence : %b\n" (precedence = peek_precedence parser);
    match (not (peek_token_is parser Token.Semicolon)) &&  (precedence < peek_precedence parser) with 
      true -> (
        let parser = next_token parser in
        Printf.printf "infix-loop TRUE cur_token: %s left: %s\n" (Token.string_of parser.cur_token) (Expression.print left);
        let (parser, left) = parse_infix_expr parser left in
        let (parser, left) = infix_loop parser precedence left in 
        (parser, left)
      )
      | false -> (
        (parser, left)
      )

  and
  parse_grouped_expr parser : (t * Expression.t) = 
    let parser = next_token parser in
    let (parser, right) = parse_expr parser Token.LOWEST in
    Printf.printf "grouped : %s\nright: %s\n" (Token.string_of parser.cur_token) (Expression.print right);
    let parser = next_token_if parser Token.RParen in
    (* ignore RParen token  *)
    (* let parser = next_token parser in *)
    Printf.printf "grouped cur_token : %s" (Token.string_of parser.cur_token);
    (parser, right)
  and
  parse_prefix_expr parser : (t * Expression.t) = 
    let prefix_token = parser.cur_token in
    let parser = next_token parser in
    let (parser, right) = parse_expr parser Token.PREFIX in
    let expr = Ast.Expression.init_prefix prefix_token right in
    (parser, expr)
  and
  prefix (parser : t) : (t * Expression.t) = match parser.cur_token.t_type with 
    | Token.Ident -> parse_identifier parser
    | Token.Int -> parse_integer parser
    | Token.True
    | Token.False -> parse_boolean parser
    | Token.Bang
    | Token.Minus -> parse_prefix_expr parser
    | Token.LParen -> parse_grouped_expr parser
    | _ -> failwith "prefix not found"
  
  and
  parse_infix_expr parser left : (t * Expression.t) = 
    let infix_token = parser.cur_token in
    let precedence = cur_precedence parser in
    let parser = next_token parser in
    let (parser, right) = parse_expr parser precedence in
    Printf.printf "infix_token : %s left: %s right: %s\n" (Token.string_of infix_token) (Expression.print left) (Expression.print right);
    let expr = Ast.Expression.init_infix infix_token left right in

    (parser, expr)
  and

  infix (parser : t) (left : Expression.t) : (t * Expression.t) = match parser.cur_token.t_type with 
    | Token.Plus 
    | Token.Minus
    | Token.Asterisk
    | Token.Slash
    | Token.Eq
    | Token.NotEq
    | Token.LT
    | Token.GT -> parse_infix_expr parser left
    | _ -> failwith "infix not found"
  ;;

  let parse_expr_stmt (parser : t) : (t * Ast.Stmt.t) = 
    let cur_token = parser.cur_token in

    let (parser, expr) = parse_expr parser Token.LOWEST in

    let stmt = Ast.Stmt.Expression (Ast.ExpressionStmt.init cur_token expr) in

    match peek_token_is parser Token.Semicolon with 
      true -> 
        let parser = next_token parser in
        (parser, stmt)
      |false -> 
        (parser, stmt)
  ;;

  let rec skip_until parser t_type = match cur_token_is parser t_type with 
    | true -> parser
    | false -> 
        let parser = next_token parser in
        skip_until parser t_type
  ;;

  let parse_let_stmt parser = 
    let stmt_token = parser.cur_token in 

    let parser = next_token_if parser Token.Ident in

    let id_token = parser.cur_token in 

    let parser = next_token_if parser Token.Assign in

    (* (* Ignore assign token *) *)
    let parser = next_token parser in

    let (parser, expr) = parse_expr parser Token.LOWEST in 

    let stmt = Ast.Stmt.init stmt_token id_token expr in
    let parser = next_token_if parser Token.Semicolon in

    (parser, stmt)
  ;;

  let parse_return_stmt parser = 
    let stmt_token = parser.cur_token in
    let parser = next_token parser in
    let id_token = parser.cur_token in 
    let (parser, expr) = parse_expr parser Token.LOWEST in 

    let stmt = Ast.Stmt.init stmt_token id_token expr in
    let parser = next_token_if parser Token.Semicolon in

    (parser, stmt)
  ;;

  let parse_stmt parser = match parser.cur_token with
    | {t_type = Token.Let; _} -> parse_let_stmt parser
    | {t_type = Token.Return; _} -> parse_return_stmt parser
    | _ -> parse_expr_stmt parser
  ;;

  let rec parse_program parser program = 
    let cur = parser.cur_token.t_type in 
    let peek = parser.peek_token.t_type in 
    match (cur, peek) with
    | (Token.EOF, _) -> List.rev program
    | (_, _) -> 
      let (parser, stmt) = parse_stmt parser in
      let program = stmt :: program in
      let parser = next_token parser in
      parse_program parser program
  ;;

end
