include Token


module type Node = sig
  type t
  val token_literal : t -> string
end

module Identifier = struct
  type t = {
    token : Token.t;
    name : string;
  }
  [@@deriving show]

  let of_token token = 
    {token; name = token.literal}
  ;;

  let eq a b = 
    match (a, b) with
      | ({token = a_token; name = a_name}, {token = b_token; name = b_name}) -> 
        (Token.eq a_token b_token) && (a_name = b_name)
  ;;

  let string_of id = 
      Printf.sprintf "Identifier: {Token: (%s) Name: %s" (Token.string_of id.token) id.name
  ;;
  let print stmt = 
    print_endline (string_of stmt)
  ;;
end
(* type stmt *)
type expr


module LetStmt = struct
  type t = {
    token : Token.t;
    id : Identifier.t;
    (* value : expr; *)
  }
    [@@deriving show];;

  let token_literal = "let"

  let t_type = Token.Let;;

  let init stmt_token id_token = 
    {token = stmt_token; id = {token = id_token; name = id_token.literal}}
  ;;
  let eq a b = 
    match (a, b) with
    | ({token = a_token; id = a_id}, {token = b_token; id = b_id}) -> 
      (Token.eq a_token b_token) && (Identifier.eq a_id b_id)
  
  ;;

  let of_identifier id = 
    {token = {t_type; literal = token_literal }; id=id}
  ;;

  let string_of stmt = 
      Printf.sprintf "LetStmt: {Token: (%s) Id: (%s)" (Token.string_of stmt.token) (Identifier.string_of stmt.id)
  ;;
  let print stmt = 
    print_endline (string_of stmt)
  ;;

end

module Stmt = struct
  type t = Let of LetStmt.t
  [@@deriving show]
    
  let token_literal _ = "";;

  let init (stmt_token : Token.t) (id_token : Token.t) = 
    match stmt_token.t_type with
    | Token.Let -> Let (LetStmt.init stmt_token id_token)
    | _ -> failwith "invalid stmt type"
  ;;

  let eq a b = 
    match (a, b) with
    | (Let a, Let b) -> LetStmt.eq a b

  let string_of = function
    | Let stmt -> LetStmt.string_of stmt

  let print stmt = 
    print_endline (string_of stmt)
  ;;
end

type program = Stmt.t list 

let rec print_program = function 
  | [] -> print_endline "EOF\n"
  | hd :: tl -> 
    let stmt = Stmt.string_of hd in
    let () = print_endline ("Stmt: " ^ stmt) in
    print_program tl
