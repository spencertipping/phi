module rec PhiVal : sig
  type t =
  | Nil
  | Int    of int
  | String of string
  | Symbol of int * string
  | Method of int * t
  | Call   of t * t

  (* TODO: entry points *)
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
  let str_method s v = Method (Hashtbl.hash s, v)
  let sym_method m v =
    match m with
    | Symbol (mh, _) -> Method (mh, v)
    | _              -> raise (PhiNotASymbolExn m)

  let nil_type    = Int 0
  let int_type    = Int 1
  let string_type = Int 2
  let symbol_type = Int 3
  let method_type = Int 4
  let call_type   = Int 5

  let val_type v = match v with
  | Nil      -> nil_type
  | Int _    -> int_type
  | String _ -> string_type
  | Symbol _ -> symbol_type
  | Method _ -> method_type
  | Call _   -> call_type
end
