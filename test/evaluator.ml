open Monkey

(* let ( let* ) x f = match x with *)
(*   | Ok x -> f x *)
(*   | Error err -> failwith (EvaluationError.string_of err) *)
(* ;; *)
let ( let* ) x f = match x with
  | Ok x -> f x
  (* | Error err -> failwith (ParseError.string_of err) *)
  | Error err -> Monkey.handle_errors err

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

let () =
  Alcotest.run "Parsing" [
    "primitive eval", [ 
      Alcotest.test_case "int" `Quick test_int_eval;
      Alcotest.test_case "bool" `Quick test_bool_eval;
    ];
  ]
