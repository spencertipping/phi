module rec PhiVal : sig
  type t =
  | Nil
  | Int    of int
  | String of string
  | Symbol of int * string
  | Method of int * t
  | Call   of t * t

  val str_symbol : string -> t
  val sym_method : t -> t -> t
  val str_method : string -> t -> t
end = struct
  type t =
  | Nil
  | Int    of int
  | String of string
  | Symbol of int * string
  | Method of int * t
  | Call   of t * t

  exception PhiNotASymbolExn of PhiVal.t

  let str_symbol s = Symbol (Hashtbl.hash s, s)
  let sym_method m v =
    match m with
    | Symbol (mh, _) -> Method (mh, v)
    | _              -> raise (PhiNotASymbolExn m)

  let str_method s v = Method (Hashtbl.hash s, v)
end


