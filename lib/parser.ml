include Token
include Lex
include Ast

module Parser = struct 
  type t = {
    lexer : Lexer.t;
    cur_token : Token.t;
    peek_token : Token.t;
  } [@@deriving show];;

  let init code = 
    let lexer = Lexer.init code in
    let cur_token = Lexer.next_token lexer in
    let peek_token = Lexer.next_token lexer in
    { lexer; cur_token; peek_token }
  
  let next_token parser = 
    let lexer = parser.lexer in
    let cur_token = parser.peek_token in
    let peek_token = Lexer.next_token parser.lexer in
    { lexer; cur_token; peek_token }
  ;;

  let cur_token_is parser t_type = 
    parser.cur_token.t_type = t_type
  ;;

  let peek_token_is parser t_type = 
    parser.peek_token.t_type = t_type
  ;;

  let next_token_if parser t_type = match (peek_token_is parser t_type) with
    | true -> next_token parser
    | false -> failwith ("failed to parse let statement\n")
  ;;


  let rec skip_until parser t_type = match cur_token_is parser t_type with 
    | true -> parser
    | false -> 
        let parser = next_token parser in
        skip_until parser t_type
    
  ;;

  let parse_let_stmt parser = 
    let token = parser.cur_token in 

    let parser = next_token_if parser Token.Ident in

    let stmt = Ast.Stmt.init token parser.cur_token in

    let parser = next_token_if parser Token.Assign in

    let parser = skip_until parser Token.Semicolon in

    (parser, stmt)
  ;;


  (* let parse_stmt _parser = [];; *)
  let parse_stmt parser = match parser.cur_token with
    | {t_type = Token.Let; _} -> parse_let_stmt parser
    | _ -> failwith "stmt not implemented"
  ;;

  let rec parse_program parser program = 
    let cur = parser.cur_token.t_type in 
    let peek = parser.peek_token.t_type in 
    match (cur, peek) with
    | (Token.EOF, _) -> program
    (* | (cur, Token.EOF) -> (cur :: program) *)
    (* | (cur, Token.EOF) -> (cur :: program) *)
    | (_, _) -> 
      let (parser, stmt) = parse_stmt parser in
      let program = stmt :: program in
      let parser = next_token parser in
      parse_program parser program
  ;;

end
