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
    let token = parser.cur_token in
    let () = print_endline ("integer_parsing: " ^ Token.string_of token) in 
    let token = parser.cur_token in
    (parser, (Ast.Expression.init token))
  ;;

  let parse_identifier (parser : t) : (t * Ast.Expression.t) =
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
  let parse_prefix parser = 
    let token = parser.cur_token in
    let () = print_endline ("prefix_parsing: " ^ Token.string_of token) in 
    let parser = next_token parser in
    (parser, (Ast.Expression.init token))
  ;;

  (* val prefix_fn : t -> Token.t -> t * Expression.t *)
  let prefix_fn parser (tok : Token.t) = match tok.t_type with 
    | Token.Int -> parse_integer parser
    | Token.Ident -> parse_identifier parser
    | Token.Bang -> parse_prefix parser
    | _ -> failwith ("failed to parse expr")
  ;;
  

  let parse_expr (parser : t) : (t * Expression.t) = 
    let cur_token = parser.cur_token in
    print_endline ("parse_expr token: " ^ Token.string_of cur_token);
    let () = print_endline ("parse_expr token: " ^ Token.string_of cur_token) in

    let (parser, expr) = prefix_fn parser cur_token in

    (parser, expr)
  ;;

  let next_token_if parser (t_type : Token.t_type) = match (peek_token_is parser t_type) with
    | true -> next_token parser
    (* | false -> failwith ("failed to parse let statement") *)
    | false -> failwith ("failed to parse statement. Expected: " ^ (Token.string_of_t_type t_type) ^ " got: " ^ Token.string_of_t_type parser.peek_token.t_type)
  ;;


  let parse_expr_stmt (parser : t) : (t * Ast.Stmt.t) = 
    let cur_token = parser.cur_token in

    let (parser, expr) = prefix_fn parser cur_token in

    print_endline ("expr_stmt expr: " ^ Expression.string_of expr);
    print_endline ("expr_stmt token: " ^ Token.string_of cur_token);
    let (parser, expr) = parse_expr parser in

    print_endline ("expr_stmt stmt: " ^ Expression.string_of expr);

    let stmt = Ast.Stmt.Expression (Ast.ExpressionStmt.init cur_token expr) in

    let parser = next_token_if parser Token.Semicolon in

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

    let id = Ast.Identifier.of_token parser.cur_token in 

    let parser = next_token_if parser Token.Assign in

    (* Ignore assign token *)
    let parser = next_token parser in

    let (parser, expr) = parse_expr parser in

    let stmt = Ast.Stmt.init stmt_token id expr in
    let parser = next_token_if parser Token.Semicolon in

    (parser, stmt)
  ;;

  let parse_return_stmt parser = 
    let stmt_token = parser.cur_token in
    let parser = next_token parser in
    let id = Ast.Identifier.of_token parser.cur_token in 
    let expr =  Ast.Expression.init parser.cur_token in 

    let stmt = Ast.Stmt.init stmt_token id expr in

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
