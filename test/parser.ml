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
      let error = Printf.sprintf "\n(%s)\n  is not equal to \n(%s)" (Stmt.string_of h) (Stmt.string_of hh) in 
      failwith error
;;

let expected_let_stmt: program = [
  Stmt.Let {
    token = {t_type = Token.Let; literal = "let"};
    id = {token = {t_type = Token.Ident; literal = "x"}; name = "x"};
    value = (Expression.init {t_type = Token.Int; literal = "5"});

  };
  Stmt.Let {
    token = {t_type = Token.Let; literal = "let"};
    id = {token = {t_type = Token.Ident; literal = "y"}; name = "y"};
    value = (Expression.init {t_type = Token.Int; literal = "10"});
  };
  Stmt.Let {
    token = {t_type = Token.Let; literal = "let"};
    id = {token = {t_type = Token.Ident; literal = "foobar"}; name = "foobar"};
    value = (Expression.init {t_type = Token.Int; literal = "838383"});
  };
];;

let expected_return_stmt: program = [
  Stmt.Return {
    token = {t_type = Token.Return; literal = "return"};
    expr = (Expression.init {t_type = Token.Int; literal = "5"});
  };
  Stmt.Return {
    token = {t_type = Token.Return; literal = "return"};
    expr = (Expression.init {t_type = Token.Int; literal = "10"});
  };
  Stmt.Return {
    token = {t_type = Token.Return; literal = "return"};
    expr = (Expression.init {t_type = Token.Int; literal = "993322"});
  };
] ;;

let expected_identity_stmt: program = [
  Stmt.Expression {
    token = {t_type = Token.Ident; literal = "foobar"};
    expr = (Expression.init {t_type = Token.Ident; literal = "foobar"});
  }
];;
let expected_integer_stmt: program = [
  Stmt.Expression {
    token = {t_type = Token.Int; literal = "5"};
    expr = (Expression.init {t_type = Token.Int; literal = "5"});
  }
];;

let expected_prefix_stmt: program = [
  Stmt.Expression {
    token = {t_type = Token.Bang; literal = "!"};
    expr = (Expression.init {t_type = Token.Int; literal = "5"});
  };
  (* Stmt.Expression { *)
  (*   token = {t_type = Token.Minus; literal = "-"}; *)
  (*   expr = (Expression.init {t_type = Token.Int; literal = "15"}); *)
  (* } *)
];;

let test_let_stmt_parser () = 
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

let test_let_stmt_string_of  () = 
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

let test_return_stmt_parser () = 
  let code = "return 5;
return 10;
return 993322;" in 
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  let () = Monkey.print_program program in 
  let is_equal = cmp program expected_return_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let test_identity_expr_parser () = 
  let code = "foobar;" in 
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  let () = Monkey.print_program program in 
  let is_equal = cmp program expected_identity_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let test_integer_expr_parser () = 
  let code = "5;" in 
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  let () = Monkey.print_program program in 
  let is_equal = cmp program expected_integer_stmt in
  Alcotest.(check bool) "is_equal" true is_equal;;
;;

let test_prefix_expr_parser () = 
(*   let code = "!5; *)
(* -15;" in  *)
  let code = "!5;" in
  let parser = Monkey.Parser.init code in 
  let program = Monkey.Parser.parse_program parser [] in 
  let () = Monkey.print_program program in 
  let is_equal = cmp program expected_prefix_stmt in
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
      Alcotest.test_case "let" `Quick test_let_stmt_parser;
      Alcotest.test_case "return" `Quick test_return_stmt_parser;
      Alcotest.test_case "identity_expr" `Quick test_identity_expr_parser;
      Alcotest.test_case "integer_expr" `Quick test_integer_expr_parser;
      Alcotest.test_case "prefix_expr" `Quick test_prefix_expr_parser;
      Alcotest.test_case "error" `Quick test_parser_error
    ];
    "String conversion", [ 
      Alcotest.test_case "let" `Quick test_let_stmt_string_of;
    ];
    (* "parsing", [  *)
    (*                     Alcotest.test_case "simple parsing" `Quick test_parser2 ; *)
    (* ]; *)
  ]
