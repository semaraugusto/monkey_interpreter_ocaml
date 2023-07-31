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
    print_endline ("old: \'" ^ (String.of_char l.ch) ^ "\'");
    if l.read_pos >= String.length l.input then
      l.ch <- '\000'
    else
      l.ch <- l.input.[l.read_pos];

    print_endline ("new: \'" ^ (String.of_char l.ch) ^ "\'");

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

  let peek l = 
    if l.read_pos >= String.length l.input then
      '\000'
    else
      l.input.[l.read_pos];;

  let rec read_identifier l output = 
    let curr = l.ch in
    let p = peek l in
    print_endline ("identifier curr: \'" ^ (String.of_char curr) ^ "\'");
    print_endline ("identifier peek: \'" ^ (String.of_char p) ^ "\'");
    
    match (curr, p) with 
    |('a' .. 'z' | 'A' .. 'Z'), ('a' .. 'z' | 'A' .. 'Z') -> 
      read_char l;
      read_identifier l (curr :: output)
    (* |('a' .. 'z' | 'A' .. 'Z'), ('(', ) ->  *)
      (* String.of_char_list (List.rev (curr :: output));; *)
    |('a' .. 'z' | 'A' .. 'Z'), _ -> 
      String.of_char_list (List.rev (curr :: output))
    | _ ->
      String.of_char_list (List.rev output);;

  let rec read_number l output = 
    let curr = l.ch in
    let p = peek l in
    print_endline ("number curr: \'" ^ (String.of_char curr) ^ "\'");
    print_endline ("number peek: \'" ^ (String.of_char p) ^ "\'");
    (* match (curr, p) with  *)
    match (curr, p) with 
    |('0' .. '9'), ('0' .. '9') -> 
    (* |'0' .. '9', '0' .. '9' ->  *)
        read_char l;
        read_number l (curr :: output);
    |('0' .. '9'), _ -> 
      String.of_char_list (List.rev (curr :: output))
    | _ ->
      String.of_char_list (List.rev output);;
    (* | '0' .. '9', (';') ->  *)
        (* let out = String.of_char_list (List.rev (curr :: output)) in *)
        (* print_endline ("number out: " ^ out); *)
        (* out  *)
    (* |'0' .. '9', (' ' | '\n') ->  *)
    (*   read_char l; *)
    (*   read_number l (curr :: output) *)

    (* (String.of_char l.ch);; *)

  let rec skip_whitespace l = 
    match l.ch with 
    | ' ' | '\n' -> 
      read_char l;
      skip_whitespace l
    | _ -> ();;
  

  let next_token l = 
      skip_whitespace l;
      let _old_ch = l.ch in
      let token = match l.ch with
    | '=' -> Token.newToken Token.Assign (Char.to_string l.ch)
    | '+' -> Token.newToken Token.Plus (Char.to_string l.ch)
    | '-' -> Token.newToken Token.Minus (Char.to_string l.ch)
    | '!' -> Token.newToken Token.Bang (Char.to_string l.ch)
    | '/' -> Token.newToken Token.Slash (Char.to_string l.ch)
    | '*' -> Token.newToken Token.Asterisk (Char.to_string l.ch)
    | '<' -> Token.newToken Token.LT (Char.to_string l.ch)
    | '>' -> Token.newToken Token.GT (Char.to_string l.ch)
    | ',' -> Token.newToken Token.Comma (Char.to_string l.ch)
    | ';' -> Token.newToken Token.Semicolon (Char.to_string l.ch)
    | '(' -> Token.newToken Token.LParen (Char.to_string l.ch)
    | ')' -> Token.newToken Token.RParen (Char.to_string l.ch)
    | '{' -> Token.newToken Token.LBrace (Char.to_string l.ch)
    | '}' -> Token.newToken Token.RBrace (Char.to_string l.ch)
    |'a' .. 'z' | 'A' .. 'Z' | '_' -> 
        let literal = (read_identifier l []) in
        (match literal with 
          "fn" -> Token.newToken Token.Function literal
          | "let" -> Token.newToken Token.Let literal
          | "if" -> Token.newToken Token.If literal
          | "else" -> Token.newToken Token.Else literal
          | "return" -> Token.newToken Token.Return literal
          | "true" -> Token.newToken Token.True literal
          | "false" -> Token.newToken Token.False literal
          | _ -> Token.newToken Token.Ident literal)
    |'0' .. '9' -> 
        let number = (read_number l []) in
        print_endline ("number: " ^ number);
        
        Token.newToken Token.Int number

    | ' ' | '\n' -> 
        failwith "space"
      (* let _ = print_endline "space" in *)
      (* read_char l; *)
      (* next_token l; *)
    | ch -> 
        Printf.printf "ch: \'%c\'\n" ch;
        Token.newToken Token.EOF "" in

    print_string ("char: " ^ (String.of_char l.ch) ^ " -> ");
    Token.print token; 
    (* if (Char.equal old_ch l.ch) then *)
      read_char l;
    (* if not (Char.equal l.ch ';') then *)
    (* print_string "next_token: "; *)
    (* Token.print token; *)
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
