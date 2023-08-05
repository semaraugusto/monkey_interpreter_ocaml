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
    let () = print_endline ("integer_parsing: " ^ Token.string_of token) in 
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

  let cur_precedence (parser : t) : Token.precedence = 
    Token.precedence_of parser.cur_token
  ;;
  let peek_precedence (parser : t) : Token.precedence = 
    Token.precedence_of parser.peek_token
  ;;


  let rec parse_prefix parser = 
    let prefix_token = parser.cur_token in
    let parser = next_token parser in
    (* parse right expression *)
    let (parser, right) = prefix_fn parser parser.cur_token in
    (* let parser = next_token_if parser Token.Semicolon in *)
    match peek_token_is parser Token.Semicolon with 
    true -> (
      let expr = Ast.Expression.init_prefix prefix_token right in
      (parser, expr)
    )
    | false -> (
      let parser = next_token parser in
      let expr = Ast.Expression.init_prefix prefix_token right in
      (parser, expr)
    )
    and 
    prefix_fn parser (tok : Token.t) = match tok.t_type with 
      | Token.Int -> parse_integer parser
      | Token.Ident -> parse_identifier parser
      | Token.Bang -> parse_prefix parser
      | Token.Minus -> parse_prefix parser
      | _ -> failwith ("failed to  parse prefix expr")
  ;;

  let rec parse_infix parser left = 
    let infix_token = parser.cur_token in

    match peek_token_is parser Token.Semicolon with 
    true -> 
      (* Printf.printf "TRUE infix_token: %s\n" (Token.string_of infix_token); *)
      (* Printf.printf "TRUE cur_token: %s\n" (Token.string_of parser.cur_token); *)
      let (parser, right) = infix_fn parser infix_token left in
      Printf.printf "true right: %s\n" (Expression.string_of right);
      (parser, right)
    | false -> 
      Printf.printf "false infix_token: %s\n" (Token.string_of infix_token);
      Printf.printf "false old: %s\n" (Token.string_of parser.cur_token);
      let parser = next_token parser in
      (* let infix_token = parser.cur_token in *)
      Printf.printf "false new: %s\n" (Token.string_of parser.cur_token);
      Printf.printf "false left: %s\n" (Expression.string_of left);
      let (parser, expr) = infix_fn parser infix_token left in
      Printf.printf "false right: %s\n" (Expression.string_of expr);
      (parser, expr)
    and   
    infix_fn parser (tok : Token.t) left = 
      let open Token in
      match tok.t_type with 
        | Plus
        | Minus
        | Asterisk
        | Eq
        | NotEq
        | LT
        | GT -> parse_infix parser left
        | Ident -> parse_identifier parser
        | Int -> parse_integer parser
        (* | GT -> failwith ("not implemented") *)
        | _ -> failwith (
            "failed to parse infix expr on token " ^ Token.string_of tok
        )
  ;;

  let parse_expr (parser : t) (_precedence : Token.precedence): (t * Expression.t) = 
    let cur_token = parser.cur_token in
    let (parser, left) = prefix_fn parser cur_token in

    if (peek_token_is parser Token.Semicolon) then (
      (parser, left)
    ) else (
      (* let precedence = cur_precedence parser in *)
      (* let cur_token = parser.cur_token in *)
      (* Printf.printf "PARSE_EXPR ELSE cur_token: %s\n" (Token.string_of cur_token); *)
      (* print_endline ("PARSE_EXPR ELSE parser.cur_token: " ^ Token.string_of parser.cur_token); *)
      let parser = next_token parser in
      let cur_token = parser.cur_token in
      (* Printf.printf "PARSE_EXPR ELSE cur_token: %s\n" (Token.string_of cur_token); *)
      (* print_endline ("PARSE_EXPR ELSE parser.cur_token: " ^ Token.string_of parser.cur_token); *)
      let (parser, right) = parse_infix parser left in
      (* Printf.printf "PARSE_EXPR  ELSE cur_token: %s\n" (Token.string_of cur_token); *)
      (* (* print_endline ("PARSE_EXPR ELSE cur_token: " ^ Token.string_of parser.cur_token); *) *)
      (* print_endline ("PARSE_EXPR ELSE parser.cur_token: " ^ Token.string_of parser.cur_token); *)
      print_endline ("parse_expr else cur_token: " ^ Token.string_of cur_token);
      let expr = Ast.Expression.init_infix cur_token left right in
      print_endline ("parse_expr else expression: " ^ Expression.string_of expr);
      (parser, expr)
    )
  ;;

  let parse_expr_stmt (parser : t) : (t * Ast.Stmt.t) = 
    let cur_token = parser.cur_token in
    print_endline ("PARSE_EXPR_STMT CUR_TOKEN: " ^ Token.string_of cur_token);
    let precedence = cur_precedence parser in

    let (parser, expr) = parse_expr parser precedence in
    print_endline ("PARSE_EXPR_STMT PARSER.CUR_TOKEN: " ^ Token.string_of parser.cur_token);
    print_endline ("PARSE_EXPR_STMT EXPR: " ^ Expression.string_of expr);

    let stmt = Ast.Stmt.Expression (Ast.ExpressionStmt.init cur_token expr) in

    print_endline ("STATEMENT!!!!!!!!: " ^ Stmt.string_of stmt);

    let parser = next_token parser in
    (* (parser, Ast.Statatement.init  *)
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

    (* Ignore assign token *)
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
    let expr =  Ast.Expression.init parser.cur_token in 

    let stmt = Ast.Stmt.init stmt_token id_token expr in

    let parser = skip_until parser Token.Semicolon in

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
