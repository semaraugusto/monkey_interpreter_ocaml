open Monkey

(* let ( let* ) x f = match x with *)
(*   | Ok x -> f x *)
(*   | Error err -> failwith (EvaluationError.string_of err) *)
(* ;; *)
let ( let* ) x f = match x with
  | Ok x -> f x
  (* | Error err -> failwith (ParseError.string_of err) *)
  | Error err -> Monkey.failwith_error err

let test_int_eval () = 
  let code = ["5"; "10"; "-5"; "-10"; "5 + 5 + 5 + 5 - 10";
    "2 * 2 * 2 * 2 * 2"; "-50 + 100 + -50"; "5 * 2 + 10"; "5 + 2 * 10"; "20 + 2 * -10"; "50 / 2 * 2 + 10"; "2 * (5 + 10)"; "3 * 3 * 3 + 10"; "3 * (3 * 3) + 10"; "(5 + 10 * 2 + 15 / 3) * 2 + -10"] in 
  let expected = [5; 10; -5; -10; 10; 32; 0; 20; 25; 0; 60; 30; 37; 37; 50] in 
  (* let parser = Monkey.Parser.init code in  *)
  for i = 0 to (List.length code) - 1 do
    print_endline ("Here at  " ^ (string_of_int i));
    let src = List.nth code i in
    let* program = Monkey.eval_program src in 
    
    let is_equal = Object.eq program (Object.Integer (List.nth expected i)) in
    Alcotest.(check bool) "is_equal" true is_equal
  done
;;

let test_bool_eval () = 
  let code = [
    "false";
    "true";
    "!true"; 
    "!false"; 
    "(1 < 2) == true";  
    "(1 > 2) == false"; 
    "1 < 1"; 
    "1 > 1"; 
    "1 == 1"; 
    "1 != 1"; 
    "1 == 2"; 
    "1 != 2"
  ] in 
  let expected = [
    false; 
    true; 
    false; 
    true; 
    true; 
    true; 
    false; 
    false; 
    true; 
    false; 
    false; 
    true
  ] in 
  for i = 0 to (List.length code) - 1 do
    print_endline ("Here at  " ^ (string_of_int i));
    let* program = Monkey.eval_program (List.nth code i) in 
    print_endline (Object.string_of program);
    let is_equal = Object.eq program (Object.Boolean (List.nth expected i)) in
    Alcotest.(check bool) "is_equal" true is_equal
  done
;;

let rec do_all f src expected =
match (src, expected) with
  | [], [] -> ()
  | _, [] -> failwith "list length mismatch"
  | [], _ -> failwith "list length mismatch"
| x :: xs, e :: es-> f x e; do_all f xs es
;; 

let check_eval src expected =
  let* program = Monkey.eval_program src in 
  print_endline ("ACTUAL: " ^ (Monkey.Object.string_of program));
  print_endline ("EXPECTED: " ^ (Monkey.Object.string_of expected));
  let is_equal = Object.eq program expected in
  Alcotest.(check bool) "is_equal" true is_equal

let test_conditional_eval () = 
  let code = [ 
    "if (true) { 10 }";
    "if (false) { 10 }";
    "if (1) { 10 }";
    "if (1 < 2) { 10 }";
    "if (1 > 2) { 10 }";
    "if (1 < 2) { 10 } else { 20 }";
    "if (1 > 2) { 10 } else { 20 }";
  ] in 
  let expected = [
    Object.Integer 10;
    Object.Null;
    Object.Integer 10;
    Object.Integer 10;
    Object.Null;
    Object.Integer 10;
    Object.Integer 20;
  ] in 

  do_all check_eval code expected;
;;

let _test_return_eval () = 
  let code = [ 
    "return 10;";
    "return 10; 9;";
    "return 2 * 5; 9;";
    "9; return 2 * 5; 9;";
    "if (10 > 1) {
  if (10 > 2) {
    return 10;
  }

  return 1;
}";
  ] in 
  let expected = [
    Object.Return (Object.Integer 10);
    Object.Return (Object.Integer 10);
    Object.Return (Object.Integer 10);
    Object.Return (Object.Integer 10);
    Object.Return (Object.Integer 10);
  ] in 

  do_all check_eval code expected;
;;

let () =
  Alcotest.run "Parsing" [
    "primitive eval", [ 
      Alcotest.test_case "int" `Quick test_int_eval;
      Alcotest.test_case "bool" `Quick test_bool_eval;
      Alcotest.test_case "conditional" `Quick test_conditional_eval;
      Alcotest.test_case "return" `Quick _test_return_eval;
    ];
  ]
