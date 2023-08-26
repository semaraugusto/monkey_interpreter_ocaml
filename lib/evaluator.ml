include Object;;
include Parser;;

(* let ( let* ) x f = match x with *)
(*   | Ok x -> f x *)
(*   | Error err -> failwith (EvaluationError.string_of err) *)
(* ;; *)
type error = [
  | `CannotEvaluate of string
  | `PrefixEvaluationError of string
  | `UnknownOperator of string
  | `TypeError of string
  | `TypeMismatch of string
  | `InfixEvaluationError of string
  | `IdentifierNotFound of string
  | `Failed of string
];;
let (let*) = Result.bind

let eval_identifier (ident: Ast.Identifier.t) env = 
  match Environment.get env ident.name with 
    | Some obj -> Ok obj
    | None -> Error (`IdentifierNotFound ident.name)

let eval_integer_infix_expr left operator right = 
  match operator with 
    | "+" -> Ok (Object.Integer (left + right))
    | "-" -> Ok (Object.Integer (left - right))
    | "*" -> Ok (Object.Integer (left * right))
    | "/" -> Ok (Object.Integer (left / right))
    | "<" -> Ok (Object.Boolean (left < right))
    | ">" -> Ok (Object.Boolean (left > right))
    | "==" -> Ok (Object.Boolean (left == right))
    | "!=" -> Ok (Object.Boolean (left != right))
    | _ -> Error (`UnknownOperator (Printf.sprintf "INTEGER %s
     INTEGER" operator))
;;
let eval_boolean_infix_expr left operator right = 
  match operator with 
    | "<" -> Ok (Object.Boolean (left < right))
    | ">" -> Ok (Object.Boolean (left > right))
    | "==" -> Ok (Object.Boolean (left == right))
    | "!=" -> Ok (Object.Boolean (left != right))
    | _ -> Error (`UnknownOperator ("BOOLEAN " ^ operator ^ " BOOLEAN"))
    (* | op -> Ok (Object.Null) *)
;;

let eval_bang_expr obj = match obj with  
  | Object.Boolean true -> Ok (Object.Boolean false)
  | Object.Boolean false -> Ok (Object.Boolean true)
  | Object.Null -> Ok (Object.Boolean true)
  | _ -> Error (`UnknownOperator (Printf.sprintf "!%s" (Object.type_of obj)))
  (* | _ -> Ok (Object.Boolean false) *)
;;

let eval_minus_expr obj = match obj with 
  | Object.Integer x -> Ok (Object.Integer (-x))
  | _ -> Error (`UnknownOperator (Printf.sprintf "-%s" (Object.type_of obj)))
;;

let is_truthy = function
  | Object.Boolean false -> false
  | Object.Null -> false
  | _ -> true

let rec eval node env = 
  match node with 
    | Ast.Node.Program p -> eval_program p env
    | Ast.Node.Statement stmt -> eval_statement stmt env
    | Ast.Node.BlockStatement block -> eval_block_statement block env
    | Ast.Node.Expression expr -> eval_expr expr env
and
eval_block_statement block env =
  let rec eval_loop statements env return = 
    match statements with
      | [] -> Ok (Object.Null)
      | x::[] -> eval (Ast.Node.of_stmt x) env
      | x::xs -> 
        let* result = eval (Ast.Node.of_stmt x) env in 
        match result with 
          | Object.Return _result -> Ok (Object.Return _result)
          | _ -> eval_loop xs env (return + 1)
  in
  eval_loop block.statements env 0

and 
eval_program (statements : Ast.Stmt.t list) env =  
  let rec eval_loop (statements : Ast.Stmt.t list) env result = 
    match statements with
      | [] -> Ok result
      | x::[] -> (
        match result with 
          | Object.Return _ -> Ok result
          | _ -> eval (Ast.Node.of_stmt x) env
      )
      | x::xs -> 
        let* result = eval (Ast.Node.of_stmt x) env in 
        match result with 
          | Object.Return _result -> Ok (Object.Return _result)
          | _ -> eval_loop xs env result
  in
  eval_loop statements env Object.Null
and 
eval_expr expr env = match expr with
  | Ast.Expression.Integer expr -> Ok (Object.Integer expr.value)
  | Ast.Expression.Boolean expr -> Ok (Object.Boolean expr.value)
  | Ast.Expression.Identifier ident -> eval_identifier ident env
  | Ast.Expression.Prefix expr -> 
    let* right = eval_expr expr.right env in 
    eval_prefix_expr expr.operator right

  | Ast.Expression.Infix expr -> 
    let* left = eval_expr expr.left env in 
    let* right = eval_expr expr.right env in 
    eval_infix_expr left expr.operator right

  | Ast.Expression.If expr -> begin
    let* condition = eval_expr expr.condition env in
    if is_truthy condition then 
      eval (Node.BlockStatement expr.consequence) env
    else 
      match expr.alternative with 
        | Some alternative -> eval (Node.BlockStatement alternative) env
        | None -> Ok (Object.Null)
    end
  | Ast.Expression.Function { token=_; parameters; body; } -> 
      Ok (Object.Function { params=parameters; body; })


  | _ -> Error (`CannotEvaluate "eval_expr not implemented for this type of expression")

and
eval_statement stmt env = match stmt with 
  | Ast.Stmt.Expression stmt -> eval_expr stmt.expr env
  | Ast.Stmt.Return stmt -> 
      let* return_value = eval_expr stmt.expr env in 
      (* print_endline ("return_statement: " ^ Object.string_of return_value); *)
      
      Ok (Object.Return return_value)

  (* | Ast.Stmt.Let stmt -> eval_expr stmt.expr *)
  | Ast.Stmt.Let stmt -> 
      let* value = eval_expr stmt.value env in 
      Environment.set env stmt.id.name value;
      Ok value

    (* Error (`CannotEvaluate "eval_statement not implemented for LET statement") *)
and
eval_prefix_expr operator right = 
  match operator with 
  | "!" -> eval_bang_expr right
  | "-" -> eval_minus_expr right
  | _ -> Error (`UnknownOperator (operator ^ (Object.string_of right)))
and
eval_infix_expr left operator right = 
  match (left, right) with 
  | (Object.Integer left, Object.Integer right) -> eval_integer_infix_expr left operator right
  | (Object.Boolean left, Object.Boolean right) -> eval_boolean_infix_expr left operator right
  | (left, right) -> Error (`TypeMismatch (Printf.sprintf "%s %s %s" (Object.type_of left) operator (Object.type_of right)))
;;

let eval_program code = 
  let parser = Parser.init code in 
  let* program = Parser.parse parser [] in 
  let env = Environment.new_env () in 
  print_endline "-------------------------------";
  print_endline (Node.pp program);
  print_endline "-------------------------------";
  (* let result = eval program in  *)
  (* result *)
  eval program env
