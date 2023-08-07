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
      (* let error = Printf.sprintf "\n(%s)\n  is not equal to \n" (Stmt.string_of h) in  *)
      failwith error
;;

let expected_let_stmt: program = [
  Stmt.Let {
    token = {t_type = Token.Let; literal = "let"};
    id = {token = {t_type = Token.Ident; literal = "x"}; name = "x"};
    value = (Expression.Integer {token = {t_type = Token.Int; literal = "5"}; value = 5; });

  };
  Stmt.Let {
    token = {t_type = Token.Let; literal = "let"};
    id = {token = {t_type = Token.Ident; literal = "y"}; name = "y"};
    value = (Expression.Integer {token = {t_type = Token.Int; literal = "10"}; value = 10; });
  };
  Stmt.Let {
    token = {t_type = Token.Let; literal = "let"};
    id = {token = {t_type = Token.Ident; literal = "foobar"}; name = "foobar"};
    value = (Expression.Integer {token = {t_type = Token.Int; literal = "838383"}; value = 838383; });
  };
];;

let expected_return_stmt: program = [
  Stmt.Return {
    token = {t_type = Token.Return; literal = "return"};
    expr = (Expression.Integer {token = {t_type = Token.Int; literal = "5"}; value = 5; });
  };
  Stmt.Return {
    token = {t_type = Token.Return; literal = "return"};
    expr = (Expression.Integer {token = {t_type = Token.Int; literal = "10"}; value = 10; });
  };
  Stmt.Return {
    token = {t_type = Token.Return; literal = "return"};
    expr = (Expression.Integer {token = {t_type = Token.Int; literal = "993322"}; value = 993322; });
  };
] ;;

let expected_identity_stmt: program = [
  Stmt.Expression {
    token = {t_type = Token.Ident; literal = "foobar"};
    expr = (Expression.Identifier {token = {t_type = Token.Ident; literal = "foobar"}; name = "foobar"});
  }
];;
let expected_integer_stmt: program = [
  Stmt.Expression {
    token = {t_type = Token.Int; literal = "5"};
    expr = (Expression.Integer {token = {t_type = Token.Int; literal = "5"}; value = 5; });
  }
];;

(* let expected_prefix_stmt: program = List.rev [ *)
let expected_prefix_stmt: program = [
  Stmt.Expression {
    token = {t_type = Token.Bang; literal = "!"};
    expr = (
      Expression.Prefix {
        token = {
          t_type = Token.Bang; 
          literal = "!"
        }; 
        operator = "!"; 
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "5"
          }; 
          value = 5; 
        }
      }
    );
  };
  Stmt.Expression {
    token = {t_type = Token.Minus; literal = "-"};
    expr = (
      Expression.Prefix {
        token = {
          t_type = Token.Minus; 
          literal = "-"
        }; 
        operator = "-"; 
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "15"
          }; 
          value = 15; 
        }
      }
    );
  };
];;

let expected_infix_stmt: program = [
  Stmt.Expression {
    (* token = {t_type = Token.Plus; literal = "+"}; *)
    token = {t_type = Token.Int; literal = "15"};
    expr = (
      Expression.Infix {
        token = {
          t_type = Token.Plus; 
          literal = "+"
        }; 
        operator = "+"; 
        left = Expression.Integer {

          token = {
            t_type = Token.Int; 
            literal = "15"
          }; 
          value = 15; 
        };
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "5"
          }; 
          value = 5; 
        }
      }
    );
  };
  Stmt.Expression {
    token = {t_type = Token.Int; literal = "15"};
    expr = (
      Expression.Infix {
        token = {
          t_type = Token.Minus; 
          literal = "-"
        }; 
        operator = "-"; 
        left = Expression.Integer {

          token = {
            t_type = Token.Int; 
            literal = "15"
          }; 
          value = 15; 
        };
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "5"
          }; 
          value = 5; 
        }
      }
    );
  };
  Stmt.Expression {
    token = {t_type = Token.Int; literal = "15"};
    expr = (
      Expression.Infix {
        token = {
          t_type = Token.Asterisk; 
          literal = "*"
        }; 
        operator = "*"; 
        left = Expression.Integer {

          token = {
            t_type = Token.Int; 
            literal = "15"
          }; 
          value = 15; 
        };
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "5"
          }; 
          value = 5; 
        }
      }
    );
  };
  Stmt.Expression {
    token = {t_type = Token.Int; literal = "15"};
    expr = (
      Expression.Infix {
        token = {
          t_type = Token.Slash; 
          literal = "/"
        }; 
        operator = "/"; 
        left = Expression.Integer {

          token = {
            t_type = Token.Int; 
            literal = "15"
          }; 
          value = 15; 
        };
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "5"
          }; 
          value = 5; 
        }
      }
    );
  };
  Stmt.Expression {
    token = {t_type = Token.Int; literal = "15"};
    expr = (
      Expression.Infix {
        token = {
          t_type = Token.LT; 
          literal = "<"
        }; 
        operator = "<"; 
        left = Expression.Integer {

          token = {
            t_type = Token.Int; 
            literal = "15"
          }; 
          value = 15; 
        };
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "5"
          }; 
          value = 5; 
        }
      }
    );
  };
  Stmt.Expression {
    token = {t_type = Token.Int; literal = "15"};
    expr = (
      Expression.Infix {
        token = {
          t_type = Token.GT; 
          literal = ">"
        }; 
        operator = ">"; 
        left = Expression.Integer {

          token = {
            t_type = Token.Int; 
            literal = "15"
          }; 
          value = 15; 
        };
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "5"
          }; 
          value = 5; 
        }
      }
    );
  };
  Stmt.Expression {
    token = {t_type = Token.Int; literal = "15"};
    expr = (
      Expression.Infix {
        token = {
          t_type = Token.Eq; 
          literal = "=="
        }; 
        operator = "=="; 
        left = Expression.Integer {

          token = {
            t_type = Token.Int; 
            literal = "15"
          }; 
          value = 15; 
        };
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "5"
          }; 
          value = 5; 
        }
      }
    );
  };
  Stmt.Expression {
    token = {t_type = Token.Int; literal = "15"};
    expr = (
      Expression.Infix {
        token = {
          t_type = Token.NotEq; 
          literal = "!="
        }; 
        operator = "!="; 
        left = Expression.Integer {

          token = {
            t_type = Token.Int; 
            literal = "15"
          }; 
          value = 15; 
        };
        right = Expression.Integer {
          token = {
            t_type = Token.Int; 
            literal = "5"
          }; 
          value = 5; 
        }
      }
    );
  };
  Stmt.Expression {
    token = {t_type = Token.Minus; literal = "5"};
    expr = (
      Expression.Infix {
        token = {
          t_type = Token.Minus; 
          literal = "-"
        }; 
        operator = "-"; 
        left = Expression.Prefix {
          token = {
            t_type = Token.Minus; 
            literal = "-"
          }; 
          operator = "-"; 
          right = Expression.Identifier {
            token = {
              t_type = Token.Ident; 
              literal = "a"
            }; 
            name = "a"; 
          }
        };
        right = Expression.Identifier {
          token = {
            t_type = Token.Ident; 
            literal = "b"
          }; 
          name = "b"; 
        }
      }
    );
  };
];;

(* let _test_let_stmt_parser () =  *)
(*   let code = " *)
(* let x = 5;" in  *)
(*   let parser = Monkey.Parser.init code in  *)
(*   let program = Monkey.Parser.parse_program parser [] in  *)
(*   let () = Monkey.print_program program in  *)
(*   let is_equal = cmp program expected_let_stmt in *)
(*   Alcotest.(check bool) "is_equal" true is_equal *)
(* ;; *)
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
-15;" in 
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
  let code = "15 + 5;
15 - 5;
15 * 5;
15 / 5;
15 < 5;
15 > 5;
15 == 5;
15 != 5;
-a - b;" in 
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
      Alcotest.test_case "error" `Quick test_parser_error
    ];
    (* "String conversion", [  *)
    (*   Alcotest.test_case "let" `Quick test_let_stmt_string_of; *)
    (* ]; *)
    (* "parsing", [  *)
    (*                     Alcotest.test_case "simple parsing" `Quick test_parser2 ; *)
    (* ]; *)
  ]
