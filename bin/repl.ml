let rec _print_list tokens = match tokens with 
        | [] -> ()
        | h::t -> Monkey.Token.print h; _print_list t

(* let ( let* ) x f : Monkey.Stmt.t list = match x with *)
(*   | Ok x -> f x *)
(*   | Error err ->  *)
(*         print_endline (Monkey.ParseError.string_of err); *)
(*         [] *)
(* ;; *)


let rec start () = 
        (* Printf.printf ">> "; *)
        (* Out_channel.output_string stdout ">> "; *)
        let line = In_channel.(input_line stdin) in 
        match line with
        | None -> start ()
        | Some line -> 
                (* print_endline line; *)
                (* let lexer = Monkey.Lexer.init line in *)
                (* let parser = Monkey.Parser.init line in  *)
                let prog = Monkey.eval_program line in 
                match prog with 
                | Error err -> print_endline (Monkey.EvaluationError.string_of err);
                | Ok prog -> 
                        print_endline (Monkey.Object.string_of prog);

                Out_channel.output_string stdout ">> ";
                start ()
;;

let () = 
        let () = print_endline "Hello! This is the Monkey programming language!" in 
        let () = print_endline "Feel free to type in commands" in
        let () = print_string ">> " in
        start ();;
        (* Out_channel.output_string stdout ">> " |> start  *)
(* let () = start () *)
