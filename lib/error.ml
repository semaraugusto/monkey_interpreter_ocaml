let handle_error error =
  match error with
  | (`CannotEvaluate expr) -> Printf.eprintf "Error: CannotEvaluate expr: %s\n" expr
  | (`Failed expr) -> Printf.eprintf "Error: SHOULD NOT BE HERE FAILED: %s\n" expr
  | (`ExpectedBoolean expr) -> Printf.eprintf "Error: Expected Boolean, found: %s\n" expr
  | (`ExpectedIdentifier expr) -> Printf.eprintf "Error: Expected Identifier, found: %s\n" expr
  | (`ExpectedInteger expr) -> Printf.eprintf "Error: Expected Integer, found: %s\n" expr
  | (`NoPrefixParseFn expr) -> Printf.eprintf "Error: NoPrefixParseFn found for expr: %s\n" expr
  | (`NoInfixParseFn expr) -> Printf.eprintf "Error: NoInfixParseFn found for expr: %s\n" expr
  | (`ExpectedSemicolon expr) -> Printf.eprintf "Error: ExpectedSemicolon at: %s\n" expr
  | (`PrefixEvaluationError expr) -> Printf.eprintf "Error: PrefixEvaluationError : %s\n" expr
  | (`InfixEvaluationError expr) -> Printf.eprintf "Error: PrefixEvaluationError : %s\n" expr

let failwith_error error =
  match error with
  | (`CannotEvaluate expr) -> Printf.ksprintf failwith "Error: CannotEvaluate expr: %s\n" expr
  | (`Failed expr) -> Printf.ksprintf failwith "Error: SHOULD NOT BE HERE FAILED: %s\n" expr
  | (`ExpectedBoolean expr) -> Printf.ksprintf failwith "Error: Expected Boolean, found: %s\n" expr
  | (`ExpectedIdentifier expr) -> Printf.ksprintf failwith "Error: Expected Identifier, found: %s\n" expr
  | (`ExpectedInteger expr) -> Printf.ksprintf failwith "Error: Expected Integer, found: %s\n" expr
  | (`NoPrefixParseFn expr) -> Printf.ksprintf failwith "Error: NoPrefixParseFn found for expr: %s\n" expr
  | (`NoInfixParseFn expr) -> Printf.ksprintf failwith "Error: NoInfixParseFn found for expr: %s\n" expr
  | (`ExpectedSemicolon expr) -> Printf.ksprintf failwith "Error: ExpectedSemicolon at: %s\n" expr
  | (`PrefixEvaluationError expr) -> Printf.ksprintf failwith "Error: PrefixEvaluationError : %s\n" expr
  | (`InfixEvaluationError expr) -> Printf.ksprintf failwith "Error: PrefixEvaluationError : %s\n" expr
