module Object = struct 
  type t = 
  | Integer of int
  | Boolean of bool
  | Return of t
  | Function of {
      params: Ast.Identifier.t list;
      (* body: Ast.Stmt.t list; *)
      body: Ast.BlockStmt.t;
    }
  | Null

  let rec string_of = function 
    | Integer i -> string_of_int i
    | Boolean b -> string_of_bool b
    | Return ret -> ("ObjectReturn: " ^ (string_of ret))
    | Null -> "null"
    | _ -> ("FUNCTION: ")
  ;;

  let type_of = function 
    | Integer _ -> "INTEGER"
    | Boolean _ -> "BOOLEAN"
    | Return _ -> "RETURN"
    | Null -> "NULL"
    | _ -> "FUNCTION"
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
(* open Object *)

module Environment = struct 
  type t = (string, Object.t) Hashtbl.t
  (* type t = (string, Object.t) Hashtbl.t *)

  let new_env () = Hashtbl.create 1;;

  let get env key = Hashtbl.find_opt env key;;

  let set env key value = Hashtbl.replace env key value;;

  let add env key value = Hashtbl.add env key value;;

  let remove env key = Hashtbl.remove env key;;
end
