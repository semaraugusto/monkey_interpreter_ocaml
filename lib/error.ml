let handle_errors (errors : 'a) =
  match errors with
  | (`CannotEvaluate expr) -> Printf.eprintf "Error: CannotEvaluate expr: %s\n" expr
  | (`Failed expr) -> Printf.eprintf "Error: SHOULD NOT BE HERE FAILED: %s\n" expr
  | (`ExpectedBoolean expr) -> Printf.eprintf "Error: Expected Boolean, found: %s\n" expr
  | (`ExpectedIdentifier expr) -> Printf.eprintf "Error: Expected Identifier, found: %s\n" expr
  | (`ExpectedInteger expr) -> Printf.eprintf "Error: Expected Integer, found: %s\n" expr
  | (`NoPrefixParseFn expr) -> Printf.eprintf "Error: NoPrefixParseFn found for expr: %s\n" expr
  | (`NoInfixParseFn expr) -> Printf.eprintf "Error: NoInfixParseFn found for expr: %s\n" expr
  | (`ExpectedSemicolon expr) -> Printf.eprintf "Error: ExpectedSemicolon at: %s\n" expr
