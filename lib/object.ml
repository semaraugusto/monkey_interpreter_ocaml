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

