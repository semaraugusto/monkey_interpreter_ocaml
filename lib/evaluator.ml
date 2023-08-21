include Object;;
include Parser;;

module EvaluationError = struct 
  type t =
    | CannotEvaluate of string
    | Failed of string

  let string_of = function
    | CannotEvaluate msg -> "CannotEvaluate: " ^ msg
    | Failed msg -> "Failed: " ^ msg

end

(* let ( let* ) x f = match x with *)
(*   | Ok x -> f x *)
(*   | Error err -> failwith (EvaluationError.string_of err) *)
(* ;; *)
module Object = struct
  type t = 
  | Integer of int
  | Boolean of bool
  | Null

  let compare = String.compare;;

  let string_of = function 
    | Integer i -> string_of_int i
    | Boolean b -> string_of_bool b
    | Null -> "null"
  ;;

  let inspect x = string_of x;;

  let eq a b = 
    match (a, b) with
    | (Integer a, Integer b) -> a = b
    | (Boolean a, Boolean b) -> a = b
    | (Null, Null) -> true
    | _ -> false
  ;;
end

let ( let* ) x f = match x with
  | Ok x -> f x
  | Error err -> failwith (EvaluationError.string_of err)
;;

let eval_integer_infix_expr left operator right = 
  match operator with 
    | "+" -> Object.Integer (left + right)
    | "-" -> Object.Integer (left - right)
    | "*" -> Object.Integer (left * right)
    | "/" -> Object.Integer (left / right)
    | "<" -> Object.Boolean (left < right)
    | ">" -> Object.Boolean (left > right)
    | "==" -> Object.Boolean (left == right)
    | "!=" -> Object.Boolean (left != right)
    | _ -> Object.Null
;;
let eval_boolean_infix_expr left operator right = 
  match operator with 
    | "<" -> Object.Boolean (left < right)
    | ">" -> Object.Boolean (left > right)
    | "==" -> Object.Boolean (left == right)
    | "!=" -> Object.Boolean (left != right)
    | _ -> Object.Null
;;

let eval_bang_expr = function 
  | Object.Boolean true -> Object.Boolean false
  | Object.Boolean false -> Object.Boolean true
  | Object.Null -> Object.Boolean true
  | _ -> Object.Boolean false
;;

let eval_minus_expr = function 
  | Object.Integer x -> Object.Integer (-x)
  | _ -> failwith "cannot minus non integer value"
;;


let rec eval node = 
  match node with 
    | Ast.Node.Program p -> eval_statements p
    | Ast.Node.Expression expr -> eval_expr expr
    | Ast.Node.Statement stmt -> eval_statement stmt
    (* | _ -> Error (EvaluationError.Failed "here not implemented") *)
    (* | Ast.Node.Expression e -> eval_expression e *)
    (* | Ast.Node.Statement s -> eval_statement s *)

and
eval_statements (statements : Ast.Stmt.t list) = 
  let rec aux (statements : Ast.Stmt.t list) = 
    match statements with
      | [] -> Ok (Object.Null)
      | x::[] -> eval (Ast.Node.of_stmt x)
      | x::xs -> 
        let* _result = eval (Ast.Node.of_stmt x) in 
        Printf.printf "result: %s\n" (Object.inspect _result);
        aux xs
  in
  aux statements
and 
eval_expr = function 
  | Ast.Expression.Integer expr -> Ok (Object.Integer expr.value)
  | Ast.Expression.Boolean expr -> Ok (Object.Boolean expr.value)
  | Ast.Expression.Prefix expr -> 
    let* right = eval_expr expr.right in 
    Ok (eval_prefix_expr expr.operator right)

  | Ast.Expression.Infix expr -> 
    let* left = eval_expr expr.left in 
    let* right = eval_expr expr.right in 
    Ok (eval_infix_expr left expr.operator right)

  | _ -> Error (EvaluationError.CannotEvaluate "eval_expr not implemented for this type of expression")
and
eval_statement = function 
  | Ast.Stmt.Expression stmt -> eval_expr stmt.expr
  | _ -> Error (EvaluationError.CannotEvaluate "eval_statement not implemented for this type of statement")
and
eval_prefix_expr operator right = 
  match operator with 
  | "!" -> eval_bang_expr right
  | "-" -> eval_minus_expr right
  | _ -> failwith "eval_prefix_expr not implemented for this type of operator"
and
eval_infix_expr left operator right = 
  match (left, right) with 
  | (Object.Integer left, Object.Integer right) -> eval_integer_infix_expr left operator right
  | (Object.Boolean left, Object.Boolean right) -> eval_boolean_infix_expr left operator right
  (* | Object.Null -> Ok(Object.Null) *)
  | _ -> failwith "eval_infix_expr not implemented for this type of operator"
;;

let eval_program code = 
  let parser = Parser.init code in 
  let program = Parser.parse parser [] in 
  match program with 
  | Ok x -> eval x
  | Error err -> failwith (ParseError.string_of err)

  (* eval program *)
