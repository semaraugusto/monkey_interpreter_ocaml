let rec print_list tokens = match tokens with 
        | [] -> ()
        | h::t -> Monkey.Token.print h; print_list t

let rec start () = 
        (* Printf.printf ">> "; *)
        (* Out_channel.output_string stdout ">> "; *)
        let line = In_channel.(input_line stdin) in 
        match line with
        | None -> ()
        | Some line -> 
                (* print_endline line; *)
                let lexer = Monkey.Lexer.init line in
                let tokens = Monkey.Lexer.parse_input lexer [] in 
                print_list tokens;
                Out_channel.output_string stdout ">> ";
                start ();;
                (* start *)
                (* List.map tokens ~f:Monkey.Token.print; *)

                (* let _str = List.map tokens ~f:Monkey.Token.to_string in *)
                (* let ast = Parser.program Lexer.read lexbuf in *)
                (* let result = Eval.eval ast in *)
                (* let () = print_endline @@ Eval.string_of_result result in *)

let () = 
        let () = print_endline "Hello! This is the Monkey programming language!" in 
        let () = print_endline "Feel free to type in commands" in
        let _ = print_string ">> " in
        start ();;
        (* Out_channel.output_string stdout ">> " |> start  *)
(* let () = start () *)
