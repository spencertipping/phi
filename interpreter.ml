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
  exception OhComeOn of PhiVal.t

  let mksym s  = Symbol (Hashtbl.hash s, s)
  let mkmut () = Mutable (ref None)
  let mkstr s  = String (Bytes.of_string s)

  let interp d c r = Cons (d, Cons (c, Cons (r, Nil)))

  let rewrite_fn f = function
    | Cons (d, Cons (c, Cons (r, Nil))) -> f c d r
    | x                                 -> raise (OhComeOn x)

  let iquote   d c r = interp (Cons (interp d c r), d) c r
  let iunquote d c r = interp d c r
  let ieval    d c r = match d with
    | Cons (x, d') -> interp d' (Cons (x, c)) r
    | x            -> raise (OhComeOn x)

  let vfn f d c r = interp (f d) c r
  let vfn1 f d c r = match d with
    | Cons (x, d') -> f x d'
    | x            -> raise (OhComeOn x)
end
