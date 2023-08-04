open Monkey

let expected_symbols: Token.t list = [
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Plus; Token.literal = "+"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.LBrace; Token.literal = "{"};
        {Token.t_type = Token.RBrace; Token.literal = "}"};
        {Token.t_type = Token.Comma; Token.literal = ","};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.EOF; Token.literal = ""};
];;

let expected_simple: Token.t list = [
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "five"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "ten"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "add"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Function; Token.literal = "fn"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Ident; Token.literal = "x"};
        {Token.t_type = Token.Comma; Token.literal = ","};
        {Token.t_type = Token.Ident; Token.literal = "y"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.LBrace; Token.literal = "{"};
        {Token.t_type = Token.Ident; Token.literal = "x"};
        {Token.t_type = Token.Plus; Token.literal = "+"};
        {Token.t_type = Token.Ident; Token.literal = "y"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.RBrace; Token.literal = "}"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "result"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Ident; Token.literal = "add"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Ident; Token.literal = "five"};
        {Token.t_type = Token.Comma; Token.literal = ","};
        {Token.t_type = Token.Ident; Token.literal = "ten"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.EOF; Token.literal = ""};
];;

let expected_operators: Token.t list = [
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "five"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "ten"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "add"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Function; Token.literal = "fn"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Ident; Token.literal = "x"};
        {Token.t_type = Token.Comma; Token.literal = ","};
        {Token.t_type = Token.Ident; Token.literal = "y"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.LBrace; Token.literal = "{"};
        {Token.t_type = Token.Ident; Token.literal = "x"};
        {Token.t_type = Token.Plus; Token.literal = "+"};
        {Token.t_type = Token.Ident; Token.literal = "y"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.RBrace; Token.literal = "}"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "result"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Ident; Token.literal = "add"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Ident; Token.literal = "five"};
        {Token.t_type = Token.Comma; Token.literal = ","};
        {Token.t_type = Token.Ident; Token.literal = "ten"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Bang; Token.literal = "!"};
        {Token.t_type = Token.Minus; Token.literal = "-"};
        {Token.t_type = Token.Slash; Token.literal = "/"};
        {Token.t_type = Token.Asterisk; Token.literal = "*"};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.LT; Token.literal = "<"};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.GT; Token.literal = ">"};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.EOF; Token.literal = ""};
];;

let expected_conditionals: Token.t list = [
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "five"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "ten"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "add"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Function; Token.literal = "fn"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Ident; Token.literal = "x"};
        {Token.t_type = Token.Comma; Token.literal = ","};
        {Token.t_type = Token.Ident; Token.literal = "y"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.LBrace; Token.literal = "{"};
        {Token.t_type = Token.Ident; Token.literal = "x"};
        {Token.t_type = Token.Plus; Token.literal = "+"};
        {Token.t_type = Token.Ident; Token.literal = "y"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.RBrace; Token.literal = "}"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "result"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Ident; Token.literal = "add"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Ident; Token.literal = "five"};
        {Token.t_type = Token.Comma; Token.literal = ","};
        {Token.t_type = Token.Ident; Token.literal = "ten"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Bang; Token.literal = "!"};
        {Token.t_type = Token.Minus; Token.literal = "-"};
        {Token.t_type = Token.Slash; Token.literal = "/"};
        {Token.t_type = Token.Asterisk; Token.literal = "*"};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.LT; Token.literal = "<"};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.GT; Token.literal = ">"};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.If; Token.literal = "if"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.LT; Token.literal = "<"};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.LBrace; Token.literal = "{"};
        {Token.t_type = Token.Return; Token.literal = "return"};
        {Token.t_type = Token.True; Token.literal = "true"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.RBrace; Token.literal = "}"};
        {Token.t_type = Token.Else; Token.literal = "else"};
        {Token.t_type = Token.LBrace; Token.literal = "{"};
        {Token.t_type = Token.Return; Token.literal = "return"};
        {Token.t_type = Token.False; Token.literal = "false"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.RBrace; Token.literal = "}"};
        {Token.t_type = Token.EOF; Token.literal = ""};
];;

let expected_comparators: Token.t list = [
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "five"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "ten"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "add"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Function; Token.literal = "fn"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Ident; Token.literal = "x"};
        {Token.t_type = Token.Comma; Token.literal = ","};
        {Token.t_type = Token.Ident; Token.literal = "y"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.LBrace; Token.literal = "{"};
        {Token.t_type = Token.Ident; Token.literal = "x"};
        {Token.t_type = Token.Plus; Token.literal = "+"};
        {Token.t_type = Token.Ident; Token.literal = "y"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.RBrace; Token.literal = "}"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Let; Token.literal = "let"};
        {Token.t_type = Token.Ident; Token.literal = "result"};
        {Token.t_type = Token.Assign; Token.literal = "="};
        {Token.t_type = Token.Ident; Token.literal = "add"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Ident; Token.literal = "five"};
        {Token.t_type = Token.Comma; Token.literal = ","};
        {Token.t_type = Token.Ident; Token.literal = "ten"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Bang; Token.literal = "!"};
        {Token.t_type = Token.Minus; Token.literal = "-"};
        {Token.t_type = Token.Slash; Token.literal = "/"};
        {Token.t_type = Token.Asterisk; Token.literal = "*"};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.LT; Token.literal = "<"};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.GT; Token.literal = ">"};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.If; Token.literal = "if"};
        {Token.t_type = Token.LParen; Token.literal = "("};
        {Token.t_type = Token.Int; Token.literal = "5"};
        {Token.t_type = Token.LT; Token.literal = "<"};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.RParen; Token.literal = ")"};
        {Token.t_type = Token.LBrace; Token.literal = "{"};
        {Token.t_type = Token.Return; Token.literal = "return"};
        {Token.t_type = Token.True; Token.literal = "true"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.RBrace; Token.literal = "}"};
        {Token.t_type = Token.Else; Token.literal = "else"};
        {Token.t_type = Token.LBrace; Token.literal = "{"};
        {Token.t_type = Token.Return; Token.literal = "return"};
        {Token.t_type = Token.False; Token.literal = "false"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.RBrace; Token.literal = "}"};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.Eq; Token.literal = "=="};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};
        {Token.t_type = Token.Int; Token.literal = "10"};
        {Token.t_type = Token.NotEq; Token.literal = "!="};
        {Token.t_type = Token.Int; Token.literal = "9"};
        {Token.t_type = Token.Semicolon; Token.literal = ";"};

        {Token.t_type = Token.EOF; Token.literal = ""};
];;

let rec cmp lst1 lst2 =
match (lst1,lst2) with
| [], [] -> true
| [],_ -> false
| _,[] -> false
| (h :: t), (hh:: tt) -> 
        Out_channel.output_string stdout "T1: ";
        Token.print h;
        Out_channel.output_string stdout "T2: ";
        Token.print hh;
        Out_channel.output_string stdout "\n";
        let is_equal = Token.eq h hh in 
        if is_equal then
                cmp t tt
        else begin
                        (* Out_channel.output_string stdout "T1: "; *)
                        (* Token.print h; *)
                        (* Out_channel.output_string stdout "T2: "; *)
                        (* Token.print hh; *)
                        (* Out_channel.output_string stderr "\n"; *)
                                (* " not equal T2: " ^ Token.print hh); *)
                        (* Out_channel.output_string stderr ("T1: " ^ Token.print h ^ " not equal T2: " ^ Token.print hh); *)
                false
                        end

let test_symbols () = 
        let _lexer = Lexer.init "=+(){},;" in
        let _tokens = Lexer.parse_input _lexer [] in 
        let is_equal = cmp _tokens expected_symbols in
        Alcotest.(check bool) "is_equal" true is_equal;;

let test_simple () = 
        let _lexer = Lexer.init "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);" in
        let _tokens = Lexer.parse_input _lexer [] in 
        let is_equal = cmp _tokens expected_simple in
        Alcotest.(check bool) "is_equal" true is_equal;;

let test_operators () = 
        let _lexer = Lexer.init "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;" in
        let tokens = Lexer.parse_input _lexer [] in 
        let is_equal = cmp tokens expected_operators in
        Alcotest.(check bool) "is_equal" true is_equal;;

let test_conditionals () = 
        let _lexer = Lexer.init "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}
" in
        let tokens = Lexer.parse_input _lexer [] in 
        let is_equal = cmp tokens expected_conditionals in
        Alcotest.(check bool) "is_equal" true is_equal;;

let test_comparators () = 
        let lexer = Lexer.init "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
" in
        let tokens = Lexer.parse_input lexer [] in 
        let is_equal = cmp tokens expected_comparators in
        Alcotest.(check bool) "is_equal" true is_equal;;

(* Run it *)
let () =
  Alcotest.run "Lexing" [
    "symbols", [ Alcotest.test_case "lexing" `Quick test_symbols ];
    "lexing", [ 
                        Alcotest.test_case "simple" `Quick test_simple ;
                        Alcotest.test_case "operator" `Quick test_operators ;
                        Alcotest.test_case "conditional" `Quick test_conditionals ;
                        Alcotest.test_case "comparators" `Quick test_comparators ;
    ];
  ]
