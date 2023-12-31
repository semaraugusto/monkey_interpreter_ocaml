open Core

include Token

module Lexer = struct 
  type t = {
    input : string;
    mutable pos : int;
    mutable read_pos : int;
    mutable ch : char;
  }
  [@@deriving show]

  let to_string l = l.input ^ "at: (pos: " ^ (string_of_int l.pos) ^ ", read_pos: " ^ (string_of_int l.read_pos) ^ ", ch: " ^ String.of_char l.ch

  let pp ppf l = 
    Format.fprintf ppf "%s\n at: (pos: %d, read_pos: %d, ch: %c" l.input l.pos l.read_pos l.ch
  ;;
  
  let read_char l = 
    if l.read_pos >= String.length l.input then
      l.ch <- '\000'
    else
      l.ch <- l.input.[l.read_pos];

    l.pos <- l.read_pos;
    l.read_pos <- l.read_pos + 1;;

  let init input =
    let l = {
      input = input;
      pos = 0;
      read_pos = 0;
      ch = '\000';
    } in
    read_char l;
    l;;

  let peek l = 
    if l.read_pos >= String.length l.input then
      '\000'
    else
      l.input.[l.read_pos];;

  let rec read_identifier l output = 
    let curr = l.ch in
    let p = peek l in
    
    match (curr, p) with 
    |('a' .. 'z' | 'A' .. 'Z'), ('a' .. 'z' | 'A' .. 'Z') -> 
      read_char l;
      read_identifier l (curr :: output)
    |('a' .. 'z' | 'A' .. 'Z'), _ -> 
      String.of_char_list (List.rev (curr :: output))
    | _ ->
      String.of_char_list (List.rev output);;

  let rec read_number l output = 
    let curr = l.ch in
    let p = peek l in

    match (curr, p) with 
    |('0' .. '9'), ('0' .. '9') -> 
        read_char l;
        read_number l (curr :: output);
    |('0' .. '9'), _ -> 
      String.of_char_list (List.rev (curr :: output))
    | _ ->
      String.of_char_list (List.rev output);;

  let rec skip_whitespace l = 
    match l.ch with 
    | ' ' | '\n' -> 
      read_char l;
      skip_whitespace l
    | _ -> ();;
  

  let next_token l = 
      skip_whitespace l;
      let token = match l.ch with
    | '=' -> 
        (match peek l with
          | '=' -> 
            read_char l; 
            Token.init Token.Eq "=="
          | _ -> Token.init Token.Assign (Char.to_string l.ch))
    | '+' -> Token.init Token.Plus (Char.to_string l.ch)
    | '-' -> Token.init Token.Minus (Char.to_string l.ch)
    | '!' -> 
        (match peek l with
          | '=' -> 
            read_char l; 
            Token.init Token.NotEq "!="
          | _ -> Token.init Token.Bang ("!"))
    | '/' -> Token.init Token.Slash (Char.to_string l.ch)
    | '*' -> Token.init Token.Asterisk (Char.to_string l.ch)
    | '<' -> Token.init Token.LT (Char.to_string l.ch)
    | '>' -> Token.init Token.GT (Char.to_string l.ch)
    | ',' -> Token.init Token.Comma (Char.to_string l.ch)
    | ';' -> Token.init Token.Semicolon (Char.to_string l.ch)
    | '(' -> Token.init Token.LParen (Char.to_string l.ch)
    | ')' -> Token.init Token.RParen (Char.to_string l.ch)
    | '{' -> Token.init Token.LBrace (Char.to_string l.ch)
    | '}' -> Token.init Token.RBrace (Char.to_string l.ch)
    |'a' .. 'z' | 'A' .. 'Z' | '_' -> 
        let literal = (read_identifier l []) in
        (match literal with 
          "fn" -> Token.init Token.Function literal
          | "let" -> Token.init Token.Let literal
          | "if" -> Token.init Token.If literal
          | "else" -> Token.init Token.Else literal
          | "return" -> Token.init Token.Return literal
          | "true" -> Token.init Token.True literal
          | "false" -> Token.init Token.False literal
          | _ -> Token.init Token.Ident literal)
    |'0' .. '9' -> 
        let number = (read_number l []) in
        
        Token.init Token.Int number

    | ' ' | '\n' -> 
        failwith "space"
    | _ -> 
        Token.init Token.EOF "" in

    (* Token.print token;  *)
    read_char l;
    token;;

  let rec parse_input lexer result = 
    let token = next_token lexer in
    match token.t_type with 
      | Token.EOF -> 
        let t = Token.init Token.EOF "" in
        List.rev (t :: result)
      | ttype -> 
        let t = Token.init ttype token.literal in
        parse_input lexer (t :: result)

end
