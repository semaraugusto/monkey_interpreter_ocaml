open Monkey

let rec cmp lst1 lst2 =
  match (lst1,lst2) with
  | [], [] -> true
  | [],_ -> false
  | _,[] -> false
  | (h :: t), (hh:: tt) -> 
    Out_channel.output_string stdout "T1: ";
    Stmt.print h;
    Out_channel.output_string stdout "T2: ";
    Stmt.print hh;
    Out_channel.output_string stdout "\n";
    let is_equal = Stmt.eq h hh in 
    if is_equal then
            cmp t tt
    else
            false
;;

let expected_let_stmt: program = [
  Stmt.Let {
    token = {t_type = Token.Let; literal = "let"};
    id = {token = {t_type = Token.Ident; literal = "foobar"}; name = "foobar"}
  };
  Stmt.Let {
    token = {t_type = Token.Let; literal = "let"};
    id = {token = {t_type = Token.Ident; literal = "y"}; name = "y"}
  };
  Stmt.Let {
    token = {t_type = Token.Let; literal = "let"};
    id = {token = {t_type = Token.Ident; literal = "x"}; name = "x"}
  }
];;
let test_parser () = 
        let code = "
let x = 5;
let y = 10;
let foobar = 838383;" in
        (* let lexer = Monkey.Lexer.init code in *)
        let parser = Monkey.Parser.init code in 
        let program = Monkey.Parser.parse_program parser [] in 
        let () = Monkey.print_program program in 
        (* let () = print_endline "hello from parser" in  *)
        let is_equal = cmp program expected_let_stmt in
        Alcotest.(check bool) "is_equal" true is_equal;;
      
        (* Alcotest.(check bool) "is_equal" true false *)
;;

let test_parser_error () = 
        let code = "
let x = 5;
let y = 10;
let foobar 838383;" in
        (* let lexer = Monkey.Lexer.init code in *)
        let parser = Monkey.Parser.init code in 
        let program = Monkey.Parser.parse_program parser [] in 
        let () = Monkey.print_program program in 
        (* let () = print_endline "hello from parser" in  *)
        let is_equal = cmp program expected_let_stmt in
        Alcotest.(check bool) "is_equal" true is_equal;;

(* let test_parser2 () =  *)
(*         let () = print_endline "hello from parser" in  *)
(*         Alcotest.(check bool) "is_equal" true false *)
;;

let () =
  Alcotest.run "Parsing" [
    "letStatement", [ 
      Alcotest.test_case "simple" `Quick test_parser;
      Alcotest.test_case "error" `Quick test_parser_error
    ];
    (* "parsing", [  *)
    (*                     Alcotest.test_case "simple parsing" `Quick test_parser2 ; *)
    (* ]; *)
  ]
