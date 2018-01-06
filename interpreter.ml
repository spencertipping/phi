module rec PhiVal : sig
  type t =
  | Int     of int
  | String  of bytes
  | Symbol  of int * string
  | Mutable of t ref option
  | Nil
  | Cons    of t * t
end = struct
  type t =
  | Int     of int
  | String  of bytes
  | Symbol  of int * string
  | Mutable of t ref option
  | Nil
  | Cons    of t * t
end

module PhiInterpreter = struct
  
end
