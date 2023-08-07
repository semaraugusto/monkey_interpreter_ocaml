
module Token = struct
  type t_type = 
    | Illegal
    | EOF

    (* Identifiers + literals *)
    | Ident
    | Int

    (* Operators *)
    | Assign
    | Plus
    | Minus
    | Bang
    | Slash
    | Asterisk
    | LT
    | GT
    | Eq
    | NotEq

    (* Delimiters *)
    | Comma
    | Semicolon
    | LParen
    | RParen
    | LBrace
    | RBrace
    (* Keywords *)
    | Function
    | Return
    | If
    | Else
    | True
    | False

    | Let
  [@@deriving show]

  let string_of_t_type t_type = match t_type with
  | Illegal -> "ILLEGAL"
  | EOF -> "EOF"
  (* identifers and literals *)
  | Ident -> "IDENT"
  | Int -> "INT"
  (* | STRING -> "STRING:" ^ tok.literal *)
  (* operators *)
  | Assign -> "ASSIGN"
  | Plus -> "PLUS"
  | Minus -> "MINUS"
  | Bang -> "BANG"
  | Slash -> "SLASH"
  | Asterisk -> "ASTERISK"
  | LT -> "LESS THAN"
  | GT -> "GREATER THAN"
  | Eq -> "EQUALS"
  | NotEq -> "NOT EQUALS"
  (* delimeters *)
  | Comma -> "COMMA"
  | Semicolon -> "SEMICOLON"
  | LParen -> "LPAREN"
  | RParen -> "RPAREN"
  | LBrace -> "LBRACE"
  | RBrace -> "RBRACE"
  (* keywords *)
  | Function -> "FUNCTION"
  | If -> "IF"
  | Else -> "ELSE"
  | Return -> "RETURN"
  | Let -> "LET"
  | True -> "TRUE"
  | False -> "FALSE"
  ;;


  type t = {
    literal: string;
    t_type: t_type;
  }
  [@@deriving show]

  (* let prefixPrecedence = 7 *)
  (* let lowest = 1 *)

  type precedence = 
    | LOWEST
    | EQUALS
    | LESSGREATER
    | SUM
    | PRODUCT
    | PREFIX
    | CALL
  [@@deriving show]
  let precedence (tok : t_type) : precedence = match tok with
  | Eq
  | NotEq -> EQUALS
  | LT
  | GT -> LESSGREATER
  | Plus
  | Minus -> SUM
  | Slash
  | Asterisk -> PRODUCT
  | _ -> LOWEST
  (* | _ -> failwith ("could not find precedence for " ^ string_of_t_type tok) *)
  ;;
  let prefixPrecedence = 7
  let lowest = 1

  (* let precedence (t_type : t_type) = match t_type with *)
  (* | Eq *)
  (* | NotEq -> 2 *)
  (* | LT *)
  (* | GT -> 3 *)
  (* | Plus *)
  (* | Minus -> 4 *)
  (* | Slash *)
  (* | Asterisk -> 5 *)
  (* | LParen -> 6 *)
  (* | _ -> lowest *)

  let precedence_of (tok : t) = 
    precedence tok.t_type
  ;;

  let compare (a : t_type) (b : t_type) =
    let a_prec = precedence a in
    let b_prec = precedence b in
    compare a_prec b_prec
  ;;

  let string_of_precedence = function
    | LOWEST -> "LOWEST"
    | EQUALS -> "EQUALS"
    | LESSGREATER -> "LESSGREATER"
    | SUM -> "SUM"
    | PRODUCT -> "PRODUCT"
    | PREFIX -> "PREFIX"
    | CALL -> "CALL"



(* let END : token = { *)
(*         t_type = EOF; literal = ""};; *)

  let tokenToString tok = match tok.t_type with
  | Illegal -> "ILLEGAL:" ^ tok.literal
  | EOF -> "EOF"
  (* identifers and literals *)
  | Ident -> "IDENT:" ^ tok.literal
  | Int -> "INT:" ^ tok.literal
  (* | STRING -> "STRING:" ^ tok.literal *)
  (* operators *)
  | Assign -> "ASSIGN"
  | Plus -> "PLUS"
  | Minus -> "MINUS"
  | Bang -> "BANG"
  | Slash -> "SLASH"
  | Asterisk -> "ASTERISK"
  | LT -> "LESS THAN"
  | GT -> "GREATER THAN"
  | Eq -> "EQUALS"
  | NotEq -> "NOT EQUALS"
  (* delimeters *)
  | Comma -> "COMMA"
  | Semicolon -> "SEMICOLON"
  | LParen -> "LPAREN"
  | RParen -> "RPAREN"
  | LBrace -> "LBRACE"
  | RBrace -> "RBRACE"
  (* keywords *)
  | Function -> "FUNCTION"
  | If -> "IF"
  | Else -> "ELSE"
  | Return -> "RETURN"
  | Let -> "LET"
  | True -> "TRUE"
  | False -> "FALSE"
  ;;

  let eof = {t_type = EOF; literal = ""}

  let init t_type literal = {t_type = t_type; literal = literal}

  let eq a b = a.literal = b.literal && a.t_type = b.t_type

  let pp ppf tk = Fmt.pf ppf "Token =%s" (tokenToString tk)

  let print tk = 
        let t = tokenToString tk in
        Printf.printf "Token: %s - Literal: \'%s\'\n" t tk.literal

  let string_of tk = 
    Printf.sprintf "Token: %s, Literal: \'%s\'" (tokenToString tk) tk.literal

  let test str = print_endline str
end
