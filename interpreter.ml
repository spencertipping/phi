module rec PhiVal : sig
  type t =
  | I of int
  | S of bytes
  | Y of int * string
  | M of t option ref
  | N
  | C of t * t
end = struct
  type t =
  | I of int
  | S of bytes
  | Y of int * string
  | M of t option ref
  | N
  | C of t * t
end

module PhiInterpreter = struct
  open PhiVal

  exception OhComeOn of PhiVal.t

  let mksym s  = Y (Hashtbl.hash s, s)
  let mkmut () = M (ref None)
  let mkstr s  = S (Bytes.of_string s)

  let rec resolve v = match v with
    | M { contents = Some x } -> resolve x
    | x                       -> x

  let interp d c r = C(resolve d, C(resolve c, C(resolve r, N)))

  let rewrite_fn f = function
    | C(d, C(c, C(r, N))) -> f (resolve d) (resolve c) (resolve r)
    | x                   -> raise (OhComeOn x)

  let iquote   d c r = interp (C(C(d, C(c, C(r, N))), d)) c r
  let iunquote d c r = match d with
    | C(d', C(c', C(r', N))) -> interp d' c' r'
    | x                      -> raise (OhComeOn x)

  let ieval d c r = match d with
    | C(x, d') -> interp d' (C(x, c)) r
    | x        -> raise (OhComeOn x)

  let vfn f d c r = interp (f d) c r
  let vfn1 f d c r = match d with
    | C(x, d') -> f (resolve x) d'
    | x        -> raise (OhComeOn x)

  let vfn2 f d c r = match d with
    | C(x, C(y, d')) -> f (resolve x) (resolve y) d'
    | x              -> raise (OhComeOn x)

  let intsym  = mksym "int"
  let strsym  = mksym "string"
  let symsym  = mksym "symbol"
  let mutsym  = mksym "mut"
  let conssym = mksym "cons"
  let nilsym  = mksym "nil"

  let vtype = vfn1 (fun x d -> C((match x with
    | I _ -> intsym
    | S _ -> strsym
    | Y _ -> symsym
    | M _ -> mutsym
    | C _ -> conssym
    | N   -> nilsym), d))

  let veq = vfn2 (fun x y d -> C((if x == y then I 1 else I 0), d))
  let vcons = vfn2 (fun x y d -> C(C(x, y), d))
  let vuncons = vfn (function
    | C(C(x, y), d) -> C(x, C(y, d))
    | x             -> raise (OhComeOn x))

  let rec nth x = function
    | 0 -> x
    | n -> match x with
      | C(x, xs) -> nth xs (n-1)
      | x        -> raise (OhComeOn x)
end
