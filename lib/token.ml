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

    (* Delimiters *)
    | Comma
    | Semicolon
    | LParen
    | RParen
    | LBrace
    | RBrace
    (* Keywords *)
    | Function
    | Let


  type token = {
    literal: string;
    t_type: t_type;
  }


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
  (* delimeters *)
  | Comma -> "COMMA"
  | Semicolon -> "SEMICOLON"
  | LParen -> "LPAREN"
  | RParen -> "RPAREN"
  | LBrace -> "LBRACE"
  | RBrace -> "RBRACE"
  (* keywords *)
  | Function -> "FUNCTION"
  | Let -> "LET"

  let newToken t_type literal = {t_type = t_type; literal = literal}

  let eq a b = a.literal = b.literal && a.t_type = b.t_type

  let pp ppf tk = Fmt.pf ppf "Token =%s" (tokenToString tk)

  let print tk = 
        let t = tokenToString tk in
        Printf.printf "Token: %s - Literal: \'%s\'\n" t tk.literal
  let test str = print_endline str
end
