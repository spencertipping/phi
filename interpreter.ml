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

  (* TODO: the mutable stuff really throws a monkey wrench into this.
     For example, any pattern match against a mutable value is going to fail,
     even if it's semantically valid. OCaml really falls down here. *)

  let mksym s  = Y (Hashtbl.hash s, s)
  let mkmut () = M (ref None)
  let mkstr s  = S (Bytes.of_string s)

  let rec resolve v = match v with
    | M { contents = Some x } -> resolve x
    | x                       -> x

  let c x y = C(resolve x, resolve y)

  let interp d c r = C(resolve d, C(resolve c, C(resolve r, N)))

  let rewrite_fn f = function
    | C(d, C(c, C(r, N))) -> f (resolve d) (resolve c) (resolve r)
    | x                   -> raise (OhComeOn x)

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

  let rec nth x = function
    | 0 -> x
    | n -> match x with
      | C(x, xs) -> nth xs (n-1)
      | x        -> raise (OhComeOn x)

  let rec nthh x n = match (nth x n) with
    | C(h, t) -> h
    | x       -> raise (OhComeOn x)

  (* interpreter functions *)
  let iquote   d c r = interp (C(C(d, C(c, C(r, N))), d)) c r
  let iunquote d c r = match d with
    | C(d', C(c', C(r', N))) -> interp d' c' r'
    | x                      -> raise (OhComeOn x)

  let ieval d c r = match d with
    | C(x, d') -> interp d' (C(x, c)) r
    | x        -> raise (OhComeOn x)

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

  let restack =
    let rec fetch onto d = function
      | C(I i, is) -> C(nthh d i, fetch onto d is)
      | N          -> onto
      | x          -> raise (OhComeOn x) in
    vfn (function
    | C(xs, C(I n, d)) ->
        let d' = nth d (n+1) in
        fetch d' d xs
    | x -> raise (OhComeOn x))

  let mut  = vfn (fun d -> C(mkmut (), d))
  let mset = vfn (function
    | C(x, C(M({ contents = None } as r), d)) ->
        let () = r := Some x in
        C(M(r), d)
    | x -> raise (OhComeOn x))

  (* integer functions *)
  let ifn1 f = vfn (function
    | C(I n, d) -> C(I(f n), d)
    | x         -> raise (OhComeOn x))

  let ifn2 f = vfn (function
    | C(I a, C(I b, d)) -> C(I(f a b), d)
    | x                 -> raise (OhComeOn x))

  let iplus   = ifn2 (+)
  let ineg    = ifn1 (~-)
  let itimes  = ifn2 ( * )
  let idivmod = vfn (function
    | C(I a, C(I b, d)) -> C(I(a / b), C(I(a mod b), d))
    | x                 -> raise (OhComeOn x))

  let ilsh    = ifn2 (lsl)
  let irsh    = ifn2 (asr)
  let iand    = ifn2 (land)
  let ixor    = ifn2 (lxor)
  let iinv    = ifn1 (lnot)
  let inot    = ifn1 (fun x -> if x = 0 then 1 else 0)

  
end
