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
  Stmt.Expression {
    token = Token.init Token.Ident "a";
    expr = 
      (
      Expression.init_infix 
      (Token.init Token.Plus "+")
      (Expression.init_infix 
        (Token.init Token.Plus "+")
        (Expression.init (Token.init Token.Ident "a"))
        (Expression.init_call 
          (Token.init Token.LParen "(")
          (Expression.init (Token.init Token.Ident "add"))
          ([
            (Expression.init_infix 
              (Token.init Token.Asterisk "*")
              (Expression.init (Token.init Token.Ident "b"))
              (Expression.init (Token.init Token.Ident "c")));
          ])))
      )
      (Expression.init (Token.init Token.Ident "d"))
  };
  Stmt.Expression {
    token = Token.init Token.Ident "add";
    expr = 
      (Expression.init_call 
        (Token.init Token.LParen "(")
        (Expression.init (Token.init Token.Ident "add"))
        ([
          (Expression.init (Token.init Token.Ident "a"));
          (Expression.init (Token.init Token.Ident "b"));
          (Expression.init (Token.init Token.Int "1"));
          (Expression.init_infix 
            (Token.init Token.Asterisk "*")
            (Expression.init (Token.init Token.Int "2"))
            (Expression.init (Token.init Token.Int "3")));
          (Expression.init_infix 
            (Token.init Token.Plus "+")
            (Expression.init (Token.init Token.Int "4"))
            (Expression.init (Token.init Token.Int "5")));
          (Expression.init_call 
            (Token.init Token.LParen "(")
            (Expression.init (Token.init Token.Ident "add"))
            ([
              (Expression.init (Token.init Token.Int "6"));
              (Expression.init_infix 
                (Token.init Token.Asterisk "*")
                (Expression.init (Token.init Token.Int "7"))
                (Expression.init (Token.init Token.Int "8")));
            ]));
        ]))
  };
  Stmt.Expression {
    token = Token.init Token.Ident "add";
    expr = 
      (Expression.init_call 
        (Token.init Token.LParen "(")
        (Expression.init (Token.init Token.Ident "add"))
        ([
          (Expression.init_infix 
              (Token.init Token.Plus "+")
              (Expression.init_infix  (* (c * d) / f *)
                (Token.init Token.Plus "+")
                (Expression.init_infix  (* (c * d) / f *)
                  (Token.init Token.Plus "+")
                    (Expression.init (Token.init Token.Ident "a"))
                    (Expression.init (Token.init Token.Ident "b")))
                (Expression.init_infix  (* (c * d) / f *)
                  (Token.init Token.Slash "/")
                  (Expression.init_infix 
                    (Token.init Token.Asterisk "*")
                    (Expression.init (Token.init Token.Ident "c"))
                    (Expression.init (Token.init Token.Ident "d")))
                  (Expression.init (Token.init Token.Ident "f"))))
              (Expression.init (Token.init Token.Ident "g")))
        ]))
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
let expected_else_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.If "if";
    expr = 
      (Expression.init_if_else 
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
          };]))
        (BlockStmt.init 
          (Token.init Token.Ident "y")
          ([Stmt.Expression {
            token = Token.init Token.Ident "y";
            expr = Expression.init (Token.init Token.Ident "y");
          };])))
  };
];;

let expected_fn_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.Function "fn";
    expr = 
      (Expression.init_function 
        (Token.init Token.Function "fn")
        ([
          Identifier.of_token (Token.init Token.Ident "x");
          Identifier.of_token (Token.init Token.Ident "y");
        ])
        (BlockStmt.init 
          (Token.init Token.Ident "x")
          ([
            Stmt.Expression {
            token = Token.init Token.Ident "x";
            expr = 
              (Expression.init_infix 
                (Token.init Token.Plus "+") 
                (Expression.init (Token.init Token.Ident "x"))
                (Expression.init (Token.init Token.Ident "y")))
            };
          ])))
  };
];;

let expected_call_stmt: program = [
  Stmt.Expression {
    token = Token.init Token.Ident "add";
    expr = 
        Expression.init_call 
        (Token.init Token.LParen "(")
        (Expression.init (Token.init Token.Ident "add"))
        ([
          (Expression.init (Token.init Token.Int "1"));
          (Expression.init_infix 
            (Token.init Token.Asterisk "*")
            (Expression.init (Token.init Token.Int "2"))
            (Expression.init (Token.init Token.Int "3")));
          (Expression.init_infix 
            (Token.init Token.Asterisk "+")
            (Expression.init (Token.init Token.Int "4"))
            (Expression.init (Token.init Token.Int "5")))
        ])
  };
];;

let ( let* ) x f = match x with
  | Ok x -> f x
  (* | Error err -> failwith (ParseError.string_of err) *)
  | Error err -> Monkey.failwith_error err

(* let ( let* ) = Result.bind;; *)


let _test_let_stmt_parser () = 
  let code = "
let x = 5;
let y = 10;
let foobar = 838383;" in
  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
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
  let* program = Monkey.Parser.parse_program parser [] in 
  let is_equal = cmp program expected_return_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_identity_expr_parser () = 
  let code = "foobar;" in 
  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
  let is_equal = cmp program expected_identity_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_integer_expr_parser () = 
  let code = "5;" in 
  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
  let is_equal = cmp program expected_integer_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_prefix_expr_parser () = 
  let code = "!5;
-15;
!true;
!false;" in 
  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
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
  let* program = Monkey.Parser.parse_program parser [] in 
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
!(true == true);
a + add(b * c) + d;
add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));
add(a + b + c * d / f + g)" in

  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
  let is_equal = cmp program expected_precedence_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_boolean_expr_parser () = 
  let code = "true
false
3 > 5 == false
3 < 5 == true" in 
  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
  let is_equal = cmp program expected_boolean_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_if_stmt () = 
  let code = "if (x < y ) { x }" in 
  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
  let is_equal = cmp program expected_if_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_else_stmt () = 
  let code = "if (x < y ) { x } else { y }" in 
  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
  let is_equal = cmp program expected_else_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_fn_stmt () = 
  let code = "fn(x, y) { x + y }" in 
  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
  let is_equal = cmp program expected_fn_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let _test_call_stmt () = 
  let code = "add(1, 2 * 3, 4 + 5);" in 
  let parser = Monkey.Parser.init code in 
  let* program = Monkey.Parser.parse_program parser [] in 
  let is_equal = cmp program expected_call_stmt in
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
      Alcotest.test_case "else" `Quick _test_else_stmt;
      Alcotest.test_case "function" `Quick _test_fn_stmt;
      Alcotest.test_case "call" `Quick _test_call_stmt;
      Alcotest.test_case "error" `Quick test_parser_error
    ];
    (* "String conversion", [  *)
    (*   Alcotest.test_case "let" `Quick test_let_stmt_string_of; *)
    (* ]; *)
    (* "parsing", [  *)
    (*                     Alcotest.test_case "simple parsing" `Quick test_parser2 ; *)
    (* ]; *)
  ]
