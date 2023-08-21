open Monkey

let ( let* ) x f = match x with
  | Ok x -> f x
  | Error err -> failwith (EvaluationError.string_of err)
;;

let test_int_eval () = 
  let code = ["5"; "10"; "-5"; "-10"; "5 + 5 + 5 + 5 - 10"] in 
  let expected = [5; 10; -5; -10; 10] in 
  (* let parser = Monkey.Parser.init code in  *)
  for i = 0 to (List.length code) - 1 do
    print_endline ("Here at  " ^ (string_of_int i));
    let* program = Monkey.eval_program (List.nth code i) in 
    
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
