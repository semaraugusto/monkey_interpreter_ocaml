open Monkey
let expected_symbols: Token.token list = [
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

let expected_simple: Token.token list = [
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

(* Run it *)
let () =
  Alcotest.run "Symbols parsing" [
    "symbols-parsing", [ Alcotest.test_case "symbols parsing" `Quick test_symbols ];
    "parsing", [ Alcotest.test_case "simple parsing" `Quick test_simple ];
                ]