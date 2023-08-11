(* module Integer = struct *)
(*   type t = int *)
(*   let compare = compare *)
(*   let string_of = string_of_int *)
(*   let inspect = string_of *)

module Object = struct
  type t = 
  | Integer of int
  | Boolean of bool
  | Null

  let compare = String.compare
  let string_of x = x
  let inspect x = string_of x
end

