(* include Token *)
include Lex
include Ast

module ParseError = struct 
  type t =
    | NoPrefixParseFn of string
    | NoInfixParseFn of string
    | ExpectedSemicolon of string
    | ExpectedIdentifier of string
    | ExpectedInteger of string
    | ExpectedBoolean of string

  let string_of = function
    | NoPrefixParseFn msg -> "NoPrefixParseFn: " ^ msg
    | NoInfixParseFn msg -> "NoInfixParseFn: " ^ msg
    | ExpectedSemicolon msg -> "ExpectedSemicolon: " ^ msg
    | ExpectedIdentifier msg -> "ExpectedIdentifier: " ^ msg
    | ExpectedInteger msg -> "ExpectedIdentifier: " ^ msg
    | ExpectedBoolean msg -> "ExpectedBoolean: " ^ msg

end

let ( let* ) x f = match x with
  | Ok x -> f x
  | Error err -> Error err

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
    { lexer; cur_token; peek_token; }
  ;;
  
  let parse_integer (parser : t) =
    match (cur_token_is parser Token.Int) with 
    | false -> Error (ParseError.ExpectedInteger ("found: " ^ (Token.string_of parser.cur_token) ^ " instead"))
    | true -> 
      let token = parser.cur_token in
      Ok (parser, Ast.Expression.init token)
  ;;

  let parse_boolean (parser : t) =
    match (cur_token_is parser Token.True) || (cur_token_is parser Token.False) with 
    | false -> Error (ParseError.ExpectedBoolean ("found: " ^ (Token.string_of parser.cur_token) ^ " instead"))
    | true -> 
      let token = parser.cur_token in
      Ok (parser, Ast.Expression.init token)
  ;;

  let parse_identifier (parser : t) =
    (* assert (cur_token_is parser Token.Ident); *)
    match cur_token_is parser Token.Ident with 
    | false -> Error (ParseError.ExpectedIdentifier ("found: " ^ (Token.string_of parser.cur_token) ^ " instead"))
    | true -> 
      let token = parser.cur_token in
      Ok (parser, Ast.Expression.init token)
  ;;

  let init code = 
    let lexer = Lexer.init code in
    let cur_token = Lexer.next_token lexer in
    let peek_token = Lexer.next_token lexer in
    { lexer; cur_token; peek_token; }
  ;;

  let next_token_if parser (t_type : Token.t_type) = match (peek_token_is parser t_type) with
    | true -> next_token parser
    | false -> failwith ("failed to parse statement. Expected: " ^ (Token.string_of_t_type t_type) ^ " got: " ^ Token.string_of_t_type parser.peek_token.t_type)
  ;;


  let rec parse_parameters parser (parameters: Identifier.t list) : (t * Ast.Identifier.t list) = 
    match peek_token_is parser Token.Comma with
    true -> 
      let parser = next_token parser in 
      let parser = next_token parser in 
      let id = Identifier.of_token parser.cur_token in
      parse_parameters parser  (id :: parameters)
  | false -> 
      let parser = next_token_if parser Token.RParen in
      (parser, List.rev parameters)
  ;;
  let parse_function_parameters parser : (t * Ast.Identifier.t list) = 
    match peek_token_is parser Token.RParen with
      true -> 
        let parser = next_token parser in
        (parser, [])
    | false -> 
        let parser = next_token parser in
        let parameters = [Identifier.of_token parser.cur_token] in
        parse_parameters parser parameters
  ;;

  let cur_precedence (parser : t) = 
    Token.precedence_of parser.cur_token
  ;;
  let peek_precedence (parser : t) = 
    Token.precedence_of parser.peek_token
  ;;


  let rec parse_expr (parser : t) (precedence: Token.precedence) = 
    let* (parser, left) = prefix parser in
    Printf.printf "parse_expr : %s left: %s\n" (Token.string_of parser.cur_token) (Expression.print left);

    infix_loop parser precedence left

  and
  parse_arguments parser (parameters: Expression.t list) = 
    match peek_token_is parser Token.Comma with
    true -> 
      let parser = next_token parser in 
      let parser = next_token parser in 
      (* let id = Identifier.of_token parser.cur_token in *)
      let* (parser, arg) = parse_expr parser Token.LOWEST in
      parse_arguments parser  (arg :: parameters)
  | false -> 
      let parser = next_token_if parser Token.RParen in
      Ok (parser, List.rev parameters)
  and
  parse_call_arguments parser = 
    match peek_token_is parser Token.RParen with
      true -> 
        let parser = next_token parser in
        Ok (parser, [])
    | false -> 
        let parser = next_token parser in
        let* (parser, arg) = parse_expr parser Token.LOWEST in
        (* let parameters = [Ast.Identifier.of_token parser.cur_token] in *)
        parse_arguments parser [arg]
  and
  parse_let_stmt parser = 
    let stmt_token = parser.cur_token in 

    let parser = next_token_if parser Token.Ident in

    let id_token = parser.cur_token in 

    let parser = next_token_if parser Token.Assign in

    (* Ignore assign token *)
    let parser = next_token parser in

    let* (parser, expr) = parse_expr parser Token.LOWEST in 

    let stmt = Ast.Stmt.init stmt_token id_token expr in
    let parser = next_token_if parser Token.Semicolon in

    Ok (parser, stmt)
  and
  parse_return_stmt parser = 
    let stmt_token = parser.cur_token in
    let parser = next_token parser in
    let id_token = parser.cur_token in 
    let* (parser, expr) = parse_expr parser Token.LOWEST in 

    let stmt = Ast.Stmt.init stmt_token id_token expr in
    let parser = next_token_if parser Token.Semicolon in

    Ok (parser, stmt)
  and
  parse_stmt parser = match parser.cur_token with
    | {t_type = Token.Let; _} -> parse_let_stmt parser
    | {t_type = Token.Return; _} -> parse_return_stmt parser
    | _ -> parse_expr_stmt parser
  and
  infix_loop (parser : t) (precedence : Token.precedence) (left: Expression.t) = 
    match (not (peek_token_is parser Token.Semicolon)) &&  (precedence < peek_precedence parser) with 
      true -> (
        let parser = next_token parser in
        Printf.printf "infix-loop TRUE cur_token: %s left: %s\n" (Token.string_of parser.cur_token) (Expression.print left);
        let* (parser, left) = infix parser left in
        let* (parser, left) = infix_loop parser precedence left in 
        Ok (parser, left)
      )
      | false -> (
        Ok (parser, left)
      )

  and
  parse_grouped_expr parser = 
    let parser = next_token parser in
    let* (parser, right) = parse_expr parser Token.LOWEST in
    let parser = next_token_if parser Token.RParen in
    Ok (parser, right)
  and
  parse_prefix_expr parser = 
    let prefix_token = parser.cur_token in
    let parser = next_token parser in
    let* (parser, right) = parse_expr parser Token.PREFIX in
    let expr = Ast.Expression.init_prefix prefix_token right in
    Ok (parser, expr)
  and
  (* parse_block_stmt parser : BlockStmt.t =  *)
  parse_block_loop parser (token : Token.t) (statements : Stmt.t list) = 
    match (not (cur_token_is parser Token.RBrace) && (not (cur_token_is parser Token.EOF))) with 
      true ->
        let* (parser, stmt) = parse_stmt parser in 
        let parser = next_token parser in
        parse_block_loop parser token (stmt :: statements)
      |false -> 
        let block = BlockStmt.init token statements in
        Ok (parser, block)
  and
  parse_block_stmt parser = 
    let parser = next_token parser in
    let token = parser.cur_token in

    let* (parser, block) = parse_block_loop parser token [] in

    Ok (parser, block)
  and
  parse_function_literal parser = 
    let token = parser.cur_token in
    let parser = next_token parser in
    let parser, parameters = parse_function_parameters parser in
    let parser = next_token_if parser Token.LBrace in
    let* (parser, body) = parse_block_stmt parser in
    let literal = Ast.Expression.init_function token parameters body in
    Ok (parser, literal)
  and
  parse_call_expr parser (funct : Ast.Expression.t) = 
    let token = parser.cur_token in
    let* parser, arguments = parse_call_arguments parser in
    let expr = Ast.Expression.init_call token funct arguments in
    Ok (parser, expr)
  and
  parse_if_expr parser = 
    let _if_token = parser.cur_token in
    let parser = next_token parser in
    let* (parser, condition) = parse_expr parser Token.LOWEST in
    let parser = next_token_if parser Token.LBrace in
    let* (parser, consequence) = parse_block_stmt parser in
    match peek_token_is parser Token.Else with 
      false ->
        let expr = Expression.init_if _if_token condition consequence in
        Ok (parser, expr)
    | true -> 
        let parser = next_token parser in
        let parser = next_token_if parser Token.LBrace in
        let* (parser, alternative) = parse_block_stmt parser in
      
        let expr = Expression.init_if_else _if_token condition consequence alternative in
        Ok (parser, expr)
  and
  parse_expr_stmt (parser : t) = 
    let cur_token = parser.cur_token in

    let* (parser, expr) = parse_expr parser Token.LOWEST in
    Printf.printf "parse_expr_stmt : %s expr: %s\n" (Token.string_of cur_token) (Expression.print expr);

    let stmt = Ast.Stmt.Expression (Ast.ExpressionStmt.init cur_token expr) in

    match peek_token_is parser Token.Semicolon with 
      true -> 
        let parser = next_token parser in
        Ok (parser, stmt)
      |false -> 
        Ok (parser, stmt)
  and
  prefix (parser : t) : (t * Expression.t, ParseError.t) result = match parser.cur_token.t_type with 
    | Token.Ident -> parse_identifier parser
    | Token.Int -> parse_integer parser
    | Token.True
    | Token.False -> parse_boolean parser
    | Token.Bang
    | Token.Minus -> parse_prefix_expr parser
    | Token.LParen -> parse_grouped_expr parser
    | Token.If -> parse_if_expr parser
    | Token.Function -> parse_function_literal parser
    | _ -> failwith "prefix not found"
  
  and
  parse_infix_expr parser left = 
    let infix_token = parser.cur_token in
    let precedence = cur_precedence parser in
    let parser = next_token parser in
    let* (parser, right) = parse_expr parser precedence in
    Printf.printf "infix_token : %s left: %s right: %s\n" (Token.string_of infix_token) (Expression.print left) (Expression.print right);
    let expr = Ast.Expression.init_infix infix_token left right in

    Ok (parser, expr)
  and

  infix (parser : t) (left : Expression.t) = match parser.cur_token.t_type with 
    | Token.Plus 
    | Token.Minus
    | Token.Asterisk
    | Token.Slash
    | Token.Eq
    | Token.NotEq
    | Token.LT
    | Token.GT -> parse_infix_expr parser left
    | Token.LParen -> parse_call_expr parser left
    | _ -> failwith "infix not found"
  ;;


  let rec skip_until parser t_type = match cur_token_is parser t_type with 
    | true -> parser
    | false -> 
        let parser = next_token parser in
        skip_until parser t_type
  ;;

  let rec parse_program parser program = 
    let cur = parser.cur_token.t_type in 
    let peek = parser.peek_token.t_type in 
    match (cur, peek) with
    | (Token.EOF, _) -> Ok (List.rev program)
    | (_, _) -> 
        let* (parser, stmt) = parse_stmt parser in
        let program = stmt :: program in
        let parser = next_token parser in
        parse_program parser program
  ;;

end
