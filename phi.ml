(* phi bootstrap interpreter *)
module rec Phi : sig
  type t =
    | Int     of int
    | String  of int * string
    | Object  of int * string
    | Cons    of t * t
    | Native  of string * (t -> t option)
    | Forward of int * t option ref

  val equal = t -> t -> bool
end = struct
  type t =
    | Int     of int
    | String  of int * string
    | Object  of int * string
    | Cons    of t * t
    | Native  of string * (t -> t option)
    | Forward of int * t option ref

  let equal = (=)
end


