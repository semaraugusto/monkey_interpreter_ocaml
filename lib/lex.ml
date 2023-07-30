open Core
include Token

module Lexer = struct 
  type lex = {
    input : string;
    mutable pos : int;
    mutable read_pos : int;
    mutable ch : char;
  }

  let read_char l = 
    if l.read_pos >= String.length l.input then
      l.ch <- '\000'
    else
      l.ch <- l.input.[l.read_pos];

    (* Out_channel.output_char stdout l.ch; *)
    l.pos <- l.read_pos;
    l.read_pos <- l.read_pos + 1;;
    (* l;; *)

  let init input =
    let l = {
      input = input;
      pos = 0;
      read_pos = 0;
      ch = '\000';
    } in
    read_char l;
    l;;

  let next_token l = 
      let token = match l.ch with
    | '=' -> Token.newToken Token.Assign (Char.to_string l.ch)
    | '+' -> Token.newToken Token.Plus (Char.to_string l.ch)
    | ',' -> Token.newToken Token.Comma (Char.to_string l.ch)
    | ';' -> Token.newToken Token.Semicolon (Char.to_string l.ch)
    | '(' -> Token.newToken Token.LParen (Char.to_string l.ch)
    | ')' -> Token.newToken Token.RParen (Char.to_string l.ch)
    | '{' -> Token.newToken Token.LBrace (Char.to_string l.ch)
    | '}' -> Token.newToken Token.RBrace (Char.to_string l.ch)
    | _ -> Token.newToken Token.EOF "" in 
    (* Token.print token;  *)
    read_char l;
    token;;

  let rec parse_input lexer result = 
    let token = next_token lexer in
    match token.t_type with 
      | Token.EOF -> 
        let t = Token.newToken Token.EOF "" in
        List.rev (t :: result)
      | ttype -> 
        let t = Token.newToken ttype token.literal in
        parse_input lexer (t :: result)

end
