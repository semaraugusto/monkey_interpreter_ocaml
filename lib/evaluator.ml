include Object;;
include Parser;;

(* let ( let* ) x f = match x with *)
(*   | Ok x -> f x *)
(*   | Error err -> failwith (EvaluationError.string_of err) *)
(* ;; *)
type error = [
  | `CannotEvaluate of string
  | `PrefixEvaluationError of string
  | `InfixEvaluationError of string
  | `Failed of string
];;

module Object = struct
  type t = 
  | Integer of int
  | Boolean of bool
  | Return of t
  | Null

  let compare = String.compare;;

  let rec string_of = function 
    | Integer i -> string_of_int i
    | Boolean b -> string_of_bool b
    | Return ret -> ("ObjectReturn: " ^ (string_of ret))
    | Null -> "null"
  ;;

  let inspect x = string_of x;;

  let rec eq a b = 
    match (a, b) with
    | (Integer a, Integer b) -> a = b
    | (Boolean a, Boolean b) -> a = b
    | (Return a, Return b) -> eq a b
    | (Null, Null) -> true
    | _ -> false
  ;;
end

let (let*) = Result.bind

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
    | _ -> Ok (Object.Null)
;;
let eval_boolean_infix_expr left operator right = 
  match operator with 
    | "<" -> Ok (Object.Boolean (left < right))
    | ">" -> Ok (Object.Boolean (left > right))
    | "==" -> Ok (Object.Boolean (left == right))
    | "!=" -> Ok (Object.Boolean (left != right))
    | _ -> Ok (Object.Null)
;;

let eval_bang_expr = function 
  | Object.Boolean true -> Ok (Object.Boolean false)
  | Object.Boolean false -> Ok (Object.Boolean true)
  | Object.Null -> Ok (Object.Boolean true)
  | _ -> Ok (Object.Boolean false)
;;

let eval_minus_expr = function 
  | Object.Integer x -> Ok (Object.Integer (-x))
  | _ -> Error (`CannotEvaluate "cannot minus non integer value")
;;

let is_truthy = function
  | Object.Boolean false -> false
  | Object.Null -> false
  | _ -> true

let rec eval (node : Node.t) = 
  match node with 
    | Ast.Node.Program p -> eval_statements p
    | Ast.Node.Expression expr -> eval_expr expr
    | Ast.Node.Statement stmt -> eval_statement stmt
    | Ast.Node.BlockStatement block -> eval_statements block.statements
    (* | _ -> Error (EvaluationError.Failed "here not implemented") *)
    (* | Ast.Node.Expression e -> eval_expression e *)
    (* | Ast.Node.Statement s -> eval_statement s *)

and
eval_statements (statements : Ast.Stmt.t list) = 
  let rec eval_loop (statements : Ast.Stmt.t list) = 
    match statements with
      | [] -> Ok (Object.Null)
      | x::[] -> eval (Ast.Node.of_stmt x)
      | x::xs -> 
        let* result = eval (Ast.Node.of_stmt x) in 
        match result with 
          | Object.Return _result -> Ok (Object.Return _result)
          | _ -> eval_loop xs
  in
  eval_loop statements
and 
eval_expr = function 
  | Ast.Expression.Integer expr -> Ok (Object.Integer expr.value)
  | Ast.Expression.Boolean expr -> Ok (Object.Boolean expr.value)
  | Ast.Expression.Prefix expr -> 
    let* right = eval_expr expr.right in 
    eval_prefix_expr expr.operator right

  | Ast.Expression.Infix expr -> 
    let* left = eval_expr expr.left in 
    let* right = eval_expr expr.right in 
    eval_infix_expr left expr.operator right

  | Ast.Expression.If expr -> begin
    let* condition = eval_expr expr.condition in
    if is_truthy condition then 
      eval (Node.BlockStatement expr.consequence)
    else 
      match expr.alternative with 
        | Some alternative -> eval (Node.BlockStatement alternative)
        | None -> Ok (Object.Null)
    end

  | _ -> Error (`CannotEvaluate "eval_expr not implemented for this type of expression")

and
eval_statement = function 
  | Ast.Stmt.Expression stmt -> eval_expr stmt.expr
  | Ast.Stmt.Return stmt -> 
      let* return_value = eval_expr stmt.expr in 
      print_endline ("return_statement: " ^ Object.string_of return_value);
      
      Ok (Object.Return return_value)

  | _ -> Error (`CannotEvaluate "eval_statement not implemented for this type of statement")
and
eval_prefix_expr operator right = 
  match operator with 
  | "!" -> eval_bang_expr right
  | "-" -> eval_minus_expr right
  | _ -> Error (`PrefixEvaluationError "not implemented for this type of statement")
and
eval_infix_expr left operator right = 
  match (left, right) with 
  | (Object.Integer left, Object.Integer right) -> eval_integer_infix_expr left operator right
  | (Object.Boolean left, Object.Boolean right) -> eval_boolean_infix_expr left operator right
  | _ -> Error (`InfixEvaluationError "eval_infix_expr not implemented for this type of statement")
;;

let eval_program code = 
  let parser = Parser.init code in 
  let* program = Parser.parse parser [] in 
  print_endline "-------------------------------";
  print_endline (Node.string_of program);
  print_endline "-------------------------------";
  (* let result = eval program in  *)
  (* result *)
  eval program
