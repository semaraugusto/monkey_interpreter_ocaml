let string_of error =
  match error with
  | (`CannotEvaluate expr) -> Printf.sprintf "Error: CannotEvaluate expr: %s\n" expr
  | (`Failed expr) -> Printf.sprintf "Error: SHOULD NOT BE HERE FAILED: %s\n" expr
  | (`ExpectedBoolean expr) -> Printf.sprintf "Error: Expected Boolean, found: %s\n" expr
  | (`ExpectedIdentifier expr) -> Printf.sprintf "Error: Expected Identifier, found: %s\n" expr
  | (`ExpectedInteger expr) -> Printf.sprintf "Error: Expected Integer, found: %s\n" expr
  | (`NoPrefixParseFn expr) -> Printf.sprintf "Error: NoPrefixParseFn found for expr: %s\n" expr
  | (`NoInfixParseFn expr) -> Printf.sprintf "Error: NoInfixParseFn found for expr: %s\n" expr
  | (`ExpectedSemicolon expr) -> Printf.sprintf "Error: ExpectedSemicolon at: %s\n" expr
  | (`PrefixEvaluationError expr) -> Printf.sprintf "Error: PrefixEvaluationError : %s\n" expr
  | (`InfixEvaluationError expr) -> Printf.sprintf "Error: InfixEvaluationError : %s\n" expr
  | (`UnknownOperator expr) -> Printf.sprintf "Error: UnknownOperator : %s\n" expr
  | (`TypeError expr) -> Printf.sprintf "Error: TypeError: %s\n" expr
  | (`TypeMismatch expr) -> Printf.sprintf "Error: TypeMismatch: %s\n" expr

let handle_error error =
  Printf.eprintf "%s" (string_of error)

let failwith_error error =
  failwith (string_of error)

(* let eq (e1: ( *)
(* [< `CannotEvaluate of string *)
(* | `ExpectedBoolean of string *)
(* | `ExpectedIdentifier of string *)
(* | `ExpectedInteger of string *)
(* | `ExpectedSemicolon of string *)
(* | `Failed of string *)
(* | `InfixEvaluationError of string *)
(* | `NoInfixParseFn of string *)
(* | `NoPrefixParseFn of string *)
(* | `PrefixEvaluationError of string *)
(* | `TypeError of string *)
(* | `UnknownOperator of string * string ])) e2 = match e1, e2 with *)
(*     | err1, err2 when err1 = err2 -> true *)
(*     | _ -> false *)

let eq e1 e2 = match e1, e2 with
    | err1, err2 when err1 = err2 -> true
    | _ -> false
