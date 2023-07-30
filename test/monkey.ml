open Monkey
let _expected: Token.token list = [
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

let rec cmp lst1 lst2 =
match (lst1,lst2) with
| [], [] -> true
| [],_ -> false
| _,[] -> false
| (h :: t), (hh:: tt) -> 
        (* print_string "t1: "; *)
        (* Token.print h; *)
        (* print_string "t2: "; *)
        (* Token.print hh; *)
        let is_equal = Token.eq h hh in 
        (* assert_equal true (Token.eq h hh); *)
        (* cmp t tt *)
        if is_equal then
                (* print_endline "YAY"; *)
                cmp t tt
        else 
                failwith "111";;

let test_lexer () = 
        let _lexer = Lexer.init "=+(){},;" in
        let _tokens = Lexer.parse_input _lexer [] in 
        let is_equal = cmp _tokens _expected in
        Alcotest.(check bool) "is_equal" true is_equal;;

(* let test_lexer2 () =  *)
(*         let _lexer = Lexer.init "=+(){},;" in *)
(*         let _tokens = Lexer.parse_input _lexer [] in  *)
(*         let is_equal = cmp _tokens _expected in *)
(*         Alcotest.(check bool) "is_equal" true is_equal *)

(* Run it *)
let () =
  Alcotest.run "Symbols parsing" [
    "symbols-parsing", [ Alcotest.test_case "symbols parsing" `Quick test_lexer ];
                ]
