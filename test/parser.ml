open Monkey

let rec cmp lst1 lst2 =
  match (lst1,lst2) with
  | [], [] -> true
  | [],_ -> false
  | _,[] -> false
  | (h :: t), (hh:: tt) -> 
    let is_equal = Stmt.eq h hh in 
    if is_equal then
            cmp t tt
    else
      let error = Printf.sprintf "\n\n(%s)\n\n is not equal to \n\n(%s)\n\n" (Stmt.string_of h) (Stmt.string_of hh) in 
      (* let error = Printf.sprintf "\n(%s)\n  is not equal to \n" (Stmt.string_of h) in  *)
      failwith error
;;

let expected_let_stmt: program = [
  Stmt.Let {
    token = Token.init Token.Let "let";
    id = Identifier.of_token (Token.init Token.Ident "x");
    value = Expression.init (Token.init Token.Int "5");

  };
  Stmt.Let {
    token = Token.init Token.Let "let";
    id = Identifier.of_token (Token.init Token.Ident "y");
    value = Expression.init (Token.init Token.Int "10");
  };
  Stmt.Let {
    token = Token.init Token.Let "let";
    id = Identifier.of_token (Token.init Token.Ident "foobar");
    value = Expression.init (Token.init Token.Int "838383");
  };
];;

let expected_return_stmt: program = [
  Stmt.Return {
    token = Token.init Token.Return "return";
    expr = Expression.init (Token.init Token.Int "5");
  };
  Stmt.Return {
    token = Token.init Token.Return "return";
    expr = Expression.init (Token.init Token.Int "10");
  };
  Stmt.Return {
    token = Token.init Token.Return "return";
    expr = Expression.init (Token.init Token.Int "993322");
  };
] ;;

let expected_identity_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.Ident "foobar";
    expr = Expression.init (Token.init Token.Ident "foobar");
  }
];;
let expected_integer_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.Int "5";
    expr = Expression.init (Token.init Token.Int "5");
  }
];;

let expected_prefix_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.Bang "!";
    expr = Expression.init_prefix (Token.init Token.Bang "!") (Expression.init (Token.init Token.Int "5"));
  };
  Stmt.Expression {
    token = Token.init Token.Minus "-";
    expr = Expression.init_prefix (Token.init Token.Minus "-") (Expression.init (Token.init Token.Int "15"));
  };
  Stmt.Expression {
    token = Token.init Token.Bang "!";
    expr = Expression.init_prefix 
      (Token.init Token.Bang "!") 
      (Expression.init (Token.init Token.True "true"));
  };
  Stmt.Expression {
    token = Token.init Token.Bang "!";
    expr = Expression.init_prefix 
      (Token.init Token.Bang "!") 
      (Expression.init (Token.init Token.False "false"));
  };
];;

let expected_infix_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.Int "15";
    expr = Expression.init_infix (Token.init Token.Plus "+") (Expression.init (Token.init Token.Int "15")) (Expression.init (Token.init Token.Int "5"));
  };
  Stmt.Expression {
    token = Token.init Token.Int "15";
    expr = Expression.init_infix (Token.init Token.Minus "-") (Expression.init (Token.init Token.Int "15")) (Expression.init (Token.init Token.Int "5"));
  };
  Stmt.Expression {
    token = Token.init Token.Int "15";
    expr = Expression.init_infix (Token.init Token.Asterisk "*") (Expression.init (Token.init Token.Int "15")) (Expression.init (Token.init Token.Int "5"));
  };
  Stmt.Expression {
    token = Token.init Token.Int "15";
    expr = Expression.init_infix (Token.init Token.Slash "/") (Expression.init (Token.init Token.Int "15")) (Expression.init (Token.init Token.Int "5"));
  };
  Stmt.Expression {
    token = Token.init Token.Int "15";
    expr = Expression.init_infix (Token.init Token.LT "<") (Expression.init (Token.init Token.Int "15")) (Expression.init (Token.init Token.Int "5"));
  };
  Stmt.Expression {
    token = Token.init Token.Int "15";
    expr = Expression.init_infix (Token.init Token.GT ">") (Expression.init (Token.init Token.Int "15")) (Expression.init (Token.init Token.Int "5"));
  };
  Stmt.Expression {
    token = Token.init Token.Int "15";
    expr = Expression.init_infix (Token.init Token.Eq "==") (Expression.init (Token.init Token.Int "15")) (Expression.init (Token.init Token.Int "5"));
  };
  Stmt.Expression {
    token = Token.init Token.Int "15";
    expr = Expression.init_infix (Token.init Token.NotEq "!=") (Expression.init (Token.init Token.Int "15")) (Expression.init (Token.init Token.Int "5"));
  };
  Stmt.Expression {
    token = Token.init Token.True "true";
    expr = 
      Expression.init_infix 
        (Token.init Token.Eq "==")
        (Expression.init (Token.init Token.True "true"))
        (Expression.init (Token.init Token.True "true"));
  };
  Stmt.Expression {
    token = Token.init Token.True "true";
    expr = 
      Expression.init_infix 
        (Token.init Token.NotEq "!=")
        (Expression.init (Token.init Token.True "true"))
        (Expression.init (Token.init Token.False "false"));
  };
  Stmt.Expression {
    token = Token.init Token.False "false";
    expr = 
      Expression.init_infix 
        (Token.init Token.NotEq "==")
        (Expression.init (Token.init Token.False "false"))
        (Expression.init (Token.init Token.False "false"));
  };
];;
let expected_precedence_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.Minus "-";
    expr = Expression.init_infix 
      (Token.init Token.Asterisk "*") 
      (Expression.init_prefix (Token.init Token.Minus "-") 
        (Expression.init (Token.init Token.Ident "a"))) 
      (Expression.init (Token.init Token.Ident "b"))
  };
  Stmt.Expression {
    token = Token.init Token.Bang "!";
    expr = 
      Expression.init_prefix 
        (Token.init Token.Bang "!") 
        (Expression.init_prefix 
          (Token.init Token.Minus "-") 
          (Expression.init (Token.init Token.Ident "a")))
  };
  Stmt.Expression {
    token = Token.init Token.Ident "a";
    expr = 
      Expression.init_infix 
        (Token.init Token.Plus "+") 
        (Expression.init_infix 
          (Token.init Token.Plus "+") 
          (Expression.init (Token.init Token.Ident "a"))
          (Expression.init (Token.init Token.Ident "b")))
        (Expression.init (Token.init Token.Ident "c"))
  };
  Stmt.Expression {
    token = Token.init Token.Ident "a";
    expr = 
      Expression.init_infix 
        (Token.init Token.Minus "-") 
        (Expression.init_infix 
          (Token.init Token.Plus "+") 
          (Expression.init (Token.init Token.Ident "a"))
          (Expression.init (Token.init Token.Ident "b")))
        (Expression.init (Token.init Token.Ident "c"))
  };
  Stmt.Expression {
    token = Token.init Token.Ident "a";
    expr = 
      Expression.init_infix 
        (Token.init Token.Asterisk "*") 
        (Expression.init_infix 
          (Token.init Token.Asterisk "*") 
          (Expression.init (Token.init Token.Ident "a"))
          (Expression.init (Token.init Token.Ident "b")))
        (Expression.init (Token.init Token.Ident "c"))
  };
  Stmt.Expression {
    token = Token.init Token.Ident "a";
    expr = 
      Expression.init_infix 
        (Token.init Token.Slash "/") 
        (Expression.init_infix 
          (Token.init Token.Asterisk "*") 
          (Expression.init (Token.init Token.Ident "a"))
          (Expression.init (Token.init Token.Ident "b")))
        (Expression.init (Token.init Token.Ident "c"))
  };
  Stmt.Expression {
    token = Token.init Token.Ident "a";
    expr = 
      Expression.init_infix 
        (Token.init Token.Plus "+") 
        (Expression.init (Token.init Token.Ident "a"))
        (Expression.init_infix 
          (Token.init Token.Slash "/") 
          (Expression.init (Token.init Token.Ident "b"))
          (Expression.init (Token.init Token.Ident "c")))
  };
  Stmt.Expression {
    token = Token.init Token.Ident "a";
    expr = 
      Expression.init_infix 
        (Token.init Token.Minus "-") 
        (Expression.init_infix 
          (Token.init Token.Plus "+") 
          (Expression.init_infix 
            (Token.init Token.Plus "+") 
            (Expression.init (Token.init Token.Ident "a"))
            (Expression.init_infix 
              (Token.init Token.Asterisk "*") 
              (Expression.init (Token.init Token.Ident "b"))
              (Expression.init (Token.init Token.Ident "c"))))
          (Expression.init_infix 
            (Token.init Token.Slash "/") 
            (Expression.init (Token.init Token.Ident "d"))
            (Expression.init (Token.init Token.Ident "e"))))
          (Expression.init (Token.init Token.Ident "f"))
  };
  Stmt.Expression {
    token = Token.init Token.Int "3";
    expr = Expression.init_infix 
      (Token.init Token.Plus "+") 
      (Expression.init (Token.init Token.Int "3"))
      (Expression.init (Token.init Token.Int "4"))
  };
  Stmt.Expression {
    token = Token.init Token.Minus "-";
    expr = Expression.init_infix 
      (Token.init Token.Asterisk "*") 
      (Expression.init_prefix 
        (Token.init Token.Minus "-")
        (Expression.init (Token.init Token.Int "5")))
      (Expression.init (Token.init Token.Int "6"))
  };
  Stmt.Expression {
    token = Token.init Token.Int "5";
    expr = 
      Expression.init_infix 
        (Token.init Token.Eq "==")
        (Expression.init_infix 
          (Token.init Token.GT ">") 
          (Expression.init (Token.init Token.Int "5"))
          (Expression.init (Token.init Token.Int "4")))
        (Expression.init_infix 
          (Token.init Token.LT "<") 
          (Expression.init (Token.init Token.Int "3"))
          (Expression.init (Token.init Token.Int "4")))
  };
  Stmt.Expression {
    token = Token.init Token.Int "5";
    expr = 
      Expression.init_infix 
        (Token.init Token.NotEq "!=")
        (Expression.init_infix 
          (Token.init Token.LT "<") 
          (Expression.init (Token.init Token.Int "5"))
          (Expression.init (Token.init Token.Int "4")))
        (Expression.init_infix 
          (Token.init Token.GT ">") 
          (Expression.init (Token.init Token.Int "3"))
          (Expression.init (Token.init Token.Int "4")))
  };
  Stmt.Expression {
    token = Token.init Token.Int "3";
    expr = 
      Expression.init_infix 
        (Token.init Token.Eq "==")
        (Expression.init_infix 
          (Token.init Token.Plus "+") 
          (Expression.init (Token.init Token.Int "3"))
          (Expression.init_infix 
            (Token.init Token.Asterisk "*") 
            (Expression.init (Token.init Token.Int "4"))
            (Expression.init (Token.init Token.Int "5"))))
        (Expression.init_infix 
          (Token.init Token.Plus "+") 
          (Expression.init_infix 
            (Token.init Token.Asterisk "*") 
            (Expression.init (Token.init Token.Int "3"))
            (Expression.init (Token.init Token.Int "1")))
          (Expression.init_infix 
            (Token.init Token.Asterisk "*") 
            (Expression.init (Token.init Token.Int "4"))
            (Expression.init (Token.init Token.Int "5"))))
  };
  Stmt.Expression {
    token = Token.init Token.Int "1";
    expr = 
      (Expression.init_infix 
        (Token.init Token.Plus "+") 
        (Expression.init_infix 
          (Token.init Token.Plus "+") 
          (Expression.init (Token.init Token.Int "1"))
          (Expression.init_infix 
            (Token.init Token.Plus "+") 
            (Expression.init (Token.init Token.Int "2"))
            (Expression.init (Token.init Token.Int "3"))))) 

        (Expression.init (Token.init Token.Int "4"))
  };
  Stmt.Expression {
    token = Token.init Token.LParen "(";
    expr = 
      (Expression.init_infix 
        (Token.init Token.Asterisk "*") 
        (Expression.init_infix 
          (Token.init Token.Plus "+") 
          (Expression.init (Token.init Token.Int "5"))
          (Expression.init (Token.init Token.Int "5")))
        (Expression.init (Token.init Token.Int "2")))
  };
  Stmt.Expression {
    token = Token.init Token.Int "2";
    expr = 
      (Expression.init_infix 
        (Token.init Token.Slash "/") 
        (Expression.init (Token.init Token.Int "2")))
        (Expression.init_infix 
          (Token.init Token.Plus "+") 
          (Expression.init (Token.init Token.Int "5"))
          (Expression.init (Token.init Token.Int "5")))
  };
  Stmt.Expression {
    token = Token.init Token.Minus "-";
    expr = 
      (Expression.init_prefix 
        (Token.init Token.Minus "-") 
        (Expression.init_infix 
          (Token.init Token.Plus "+") 
          (Expression.init (Token.init Token.Int "5"))
          (Expression.init (Token.init Token.Int "5"))))
  };
  Stmt.Expression {
    token = Token.init Token.Bang "!";
    expr = 
      (Expression.init_prefix 
        (Token.init Token.Bang "!") 
        (Expression.init_infix 
          (Token.init Token.Eq "==") 
          (Expression.init (Token.init Token.True "true"))
          (Expression.init (Token.init Token.True "true"))))
  };
];;

let expected_boolean_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.True "true";
    expr = Expression.init (Token.init Token.True "true");
  };
  Stmt.Expression {
    token = Token.init Token.False "false";
    expr = Expression.init (Token.init Token.False "false");
  };
  Stmt.Expression {
    token = Token.init Token.Int "3";
    expr = 
      (Expression.init_infix 
        (Token.init Token.Eq "==") 
        (Expression.init_infix 
          (Token.init Token.GT ">")
          (Expression.init (Token.init Token.Int "3"))
          (Expression.init (Token.init Token.Int "5")))

        (Expression.init (Token.init Token.False "false")))
  };
  Stmt.Expression {
    token = Token.init Token.Int "3";
    expr = 
      (Expression.init_infix 
        (Token.init Token.Eq "==") 
        (Expression.init_infix 
          (Token.init Token.LT "<")
          (Expression.init (Token.init Token.Int "3"))
          (Expression.init (Token.init Token.Int "5")))

        (Expression.init (Token.init Token.True "true")))
  };
];;
let expected_if_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.If "if";
    expr = 
      (Expression.init_if 
        (Token.init Token.If "if")
        (Expression.init_infix 
          (Token.init Token.LT "<")
          (Expression.init (Token.init Token.Ident "x"))
          (Expression.init (Token.init Token.Ident "y")))
        (BlockStmt.init 
          (Token.init Token.Ident "x")
          ([Stmt.Expression {
            token = Token.init Token.Ident "x";
            expr = Expression.init (Token.init Token.Ident "x");
          };])))
  };
];;


let _test_let_stmt_parser () = 
  let code = "
let x = 5;
let y = 10;
let foobar = 838383;" in
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  let () = Monkey.print_program program in 
  let is_equal = cmp program expected_let_stmt in
  Alcotest.(check bool) "is_equal" true is_equal
;;

let _test_let_stmt_string_of  () = 
  let program = [
    Stmt.Let {
      token = {t_type = Token.Let; literal = "let"};
      id = {token = {t_type = Token.Ident; literal = "myvar"}; name = "myvar"};
      value = (Expression.init {t_type = Token.Ident; literal = "anotherVar"});
    }
  ] in
  let expected = "let myvar = anotherVar;" in
  let actual = Stmt.string_of (List.hd program) in
  Alcotest.(check string) "is_equal" expected actual
;;

let _test_return_stmt_parser () = 
  let code = "return 5;
return 10;
return 993322;" in 
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  let () = Monkey.print_program program in 
  let is_equal = cmp program expected_return_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_identity_expr_parser () = 
  let code = "foobar;" in 
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  let () = Monkey.print_program program in 
  let is_equal = cmp program expected_identity_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_integer_expr_parser () = 
  let code = "5;" in 
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  let () = Monkey.print_program program in 
  let is_equal = cmp program expected_integer_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_prefix_expr_parser () = 
  let code = "!5;
-15;
!true;
!false;" in 
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  print_endline "expected_program";
  let () = Monkey.print_program program in 
  print_endline "actual_program";
  let () = Monkey.print_program expected_prefix_stmt in 
  let is_equal = cmp program expected_prefix_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_infix_expr_parser () = 
(*   let code = "15 + 5; *)
(* 15 - 5; *)
(* 15 * 5; *)
(* 15 / 5; *)
(* 15 < 5; *)
(* 15 > 5; *)
(* 15 == 5; *)
(* 15 != 5; *)
(* -a - b;" in  *)
  let code = "15 + 5;
15 - 5;
15 * 5;
15 / 5;
15 < 5;
15 > 5;
15 == 5;
15 != 5;
true == true;
true != false;
false == false;" in
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  print_endline "actual_program";
  let () = Monkey.print_program program in 

  print_endline "___________________-";
  let () = match program with 
  | [] -> print_endline "empty program"
  | hd :: _tl -> print_endline (Stmt.string_of hd) in
  print_endline "___________________-";
  print_endline "expected_program";
  let () = Monkey.print_program expected_infix_stmt in 
  let is_equal = cmp program expected_infix_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_precedence_expr_parser () = 
  (* let code = "(5 + 5) * 2" in *)
  let code = "-a * b
!-a
a + b + c
a + b - c
a * b * c
a * b / c
a + b / c
a + b * c + d / e - f
3 + 4; -5 * 6
5 > 4 == 3 < 4
5 < 4 != 3 > 4
3 + 4 * 5 == 3 * 1 + 4 * 5
1 + (2 + 3) + 4;
(5 + 5) * 2;
2 / (5 + 5);
-(5 + 5);
!(true == true);" in

  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  print_endline "actual_program";
  print_endline "___________________-";
  let () = Monkey.print_program program in 
  print_endline "___________________-";

  print_endline "expected_program";
  print_endline "___________________-";
  let () = Monkey.print_program expected_precedence_stmt in 
  print_endline "___________________-";
  let strings = List.map Stmt._print program in 
  let () = match strings with 
  | [] -> print_endline "empty program"
  | hd :: _tl -> (print_endline hd) in
  print_endline "___________________-";
  print_endline "___________________-";
  let is_equal = cmp program expected_precedence_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_boolean_expr_parser () = 
  let code = "true
false
3 > 5 == false
3 < 5 == true" in 
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  print_endline "actual_program";
  print_endline "___________________-";
  let () = Monkey.print_program program in 
  print_endline "___________________-";

  print_endline "expected_program";
  print_endline "___________________-";
  let () = Monkey.print_program expected_precedence_stmt in 
  print_endline "___________________-";
  let strings = List.map Stmt._print program in 
  let () = match strings with 
  | [] -> print_endline "empty program"
  | hd :: _tl -> (print_endline hd) in
  let is_equal = cmp program expected_boolean_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_if_stmt () = 
  let code = "if (x < y ) { x }" in 
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  print_endline "actual_program";
  print_endline "___________________-";
  let () = Monkey.print_program program in 
  print_endline "___________________-";

  print_endline "expected_program";
  print_endline "___________________-";
  let () = Monkey.print_program expected_precedence_stmt in 
  print_endline "___________________-";
  let strings = List.map Stmt._print program in 
  let () = match strings with 
  | [] -> print_endline "empty program"
  | hd :: _tl -> (print_endline hd) in
  let is_equal = cmp program expected_if_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let test_parser_error () = 
        let code = "
let x = 5;
let y = 10;
let foobar 838383;" in
        (* let lexer = Monkey.Lexer.init code in *)
        let _parser = Monkey.Parser.init code in 

        (* TODO: add test *)
        ()
;;
        (* Alcotest.(check fail) Monkey.Parser.parse_program parser [] *)
;;

let () =
  Alcotest.run "Parsing" [
    "Statements", [ 
      Alcotest.test_case "let" `Quick _test_let_stmt_parser;
      Alcotest.test_case "return" `Quick _test_return_stmt_parser;
      Alcotest.test_case "identity_expr" `Quick _test_identity_expr_parser;
      Alcotest.test_case "integer_expr" `Quick _test_integer_expr_parser;
      Alcotest.test_case "prefix_expr" `Quick _test_prefix_expr_parser;
      Alcotest.test_case "infix_expr" `Quick _test_infix_expr_parser;
      Alcotest.test_case "precedence_expr" `Quick _test_precedence_expr_parser;
      Alcotest.test_case "plus_expr" `Quick _test_boolean_expr_parser;
      Alcotest.test_case "if" `Quick _test_if_stmt;
      Alcotest.test_case "error" `Quick test_parser_error
    ];
    (* "String conversion", [  *)
    (*   Alcotest.test_case "let" `Quick test_let_stmt_string_of; *)
    (* ]; *)
    (* "parsing", [  *)
    (*                     Alcotest.test_case "simple parsing" `Quick test_parser2 ; *)
    (* ]; *)
  ]
