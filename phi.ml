open Bytes

module rec PhiV : sig
  type t =
    | Nil
    | Cons of PhiV.t * PhiV.t
    | Int  of int
    | Real of float
    | Str  of bytes
    | Sym  of int * string
    | Mut  of PhiV.t option ref
end = struct
  type t =
    | Nil
    | Cons of PhiV.t * PhiV.t
    | Int  of int
    | Real of float
    | Str  of bytes
    | Sym  of int * string
    | Mut  of PhiV.t option ref
end

open PhiV

exception TotallyBogusExn
exception DerefNullMutExn
exception StackUnderflowExn
exception UnimplementedExn
exception ExpectedACons of PhiV.t
exception ExpectedAnInt of PhiV.t
exception ExpectedAList of PhiV.t
exception IllegalInsnExn of PhiV.t

type phii = PhiV.t * PhiV.t * PhiV.t

let sym_of_str s =
  let h = Hashtbl.hash s in
  Sym (h, s)

let t_nil  = sym_of_str "nil"
let t_cons = sym_of_str "cons"
let t_int  = sym_of_str "int"
let t_real = sym_of_str "real"
let t_str  = sym_of_str "str"
let t_sym  = sym_of_str "sym"
let t_mut  = sym_of_str "mut"

let rec deref = function
  | Mut { contents = Some x } -> deref x
  | Mut { contents = None }   -> raise DerefNullMutExn
  | x                         -> x

let rec typeof = function
  | Mut { contents = Some x } -> typeof x
  | Mut { contents = None }   -> t_mut
  | Nil                       -> t_nil
  | Cons _                    -> t_cons
  | Int _                     -> t_int
  | Real _                    -> t_real
  | Str _                     -> t_str
  | Sym _                     -> t_sym

let rec nthcell x = function
  | 0 -> x
  | n -> match deref x with
    | Cons (_, x') -> nthcell x' (n-1)
    | x'           -> raise (ExpectedACons x')

let rec phimap_onto e f xs = match deref xs with
  | Cons (v, xs') -> Cons (f v, phimap_onto e f xs')
  | Nil           -> e
  | x             -> raise (ExpectedAList x)

let restack d xs = match deref xs with
  | Cons (n, is) -> (match deref n with
    | Int n' ->
      phimap_onto
        (nthcell d n')
        (fun x -> match deref x with
           | Int i -> (match deref (nthcell d i) with
             | Cons (h, _) -> h
             | x           -> raise (ExpectedACons x))
           | x     -> raise (ExpectedAnInt x))
        is
    | x -> raise (ExpectedAnInt x))
  | x -> raise (ExpectedACons x)

let two_int_op f d = match deref d with
  | Cons (x, d') -> (match deref d' with
    | Cons (y, d'') -> (match (x, y) with
      | (Int x', Int y') -> Cons(f x' y', d'')
      | (Int _, _)       -> raise (ExpectedAnInt y)
      | _                -> raise (ExpectedAnInt x))
    | _             -> raise StackUnderflowExn)
  | _            -> raise StackUnderflowExn

let eval (d, c, r) insn =
  match deref insn with
    | Mut _       -> raise TotallyBogusExn
    | Nil         -> (Cons(Nil, d), c, r)
    | Cons _ as x -> (Cons(x, d),   c, r)
    | Str _  as x -> (Cons(x, d),   c, r)
    | Real _ as x -> (Cons(x, d),   c, r)

    | Sym _  as x -> (Cons(x, d), Cons(r, Cons(Int 2, c)), r)
    | Int i -> match i with
      | 0x00 -> (Cons(Cons(d, Cons(c, Cons(r, Nil))), d), c, r)
      | 0x01 -> (match deref d with
                   | Cons(c', d') -> (d', c', r)
                   | _            -> raise StackUnderflowExn)
      | 0x02 -> (match deref d with
                   | Cons(x, d') -> (d', Cons(x, c), r)
                   | _           -> raise StackUnderflowExn)
      | 0x03 -> (match deref d with
                   | Cons(x, d') -> (Cons(typeof x, d'), c, r)
                   | _           -> raise StackUnderflowExn)
      | 0x04 -> raise UnimplementedExn
      | 0x05 -> (match deref d with
                   | Cons(h, d') -> (match deref d' with
                     | Cons(t, d'') -> (Cons(Cons(h, t), d''), c, r)
                     | _            -> raise StackUnderflowExn)
                   | _           -> raise StackUnderflowExn)
      | 0x06 -> (match deref d with
                   | Cons(x, d') -> (match deref d' with
                     | Cons(h, t) -> (Cons(h, Cons(t, d')), c, r)
                     | x          -> raise (ExpectedACons x))
                   | _           -> raise StackUnderflowExn)
      | 0x07 -> (match deref d with
                   | Cons(x, d') -> (restack d' x, c, r)
                   | x           -> raise (ExpectedACons x))
      | 0x08 -> (Cons(Mut (ref None), d), c, r)
      | 0x09 -> (match deref d with
                   | Cons(v, d') -> (match deref d' with
                     | Cons(Mut m as x, d'') -> m := Some v; (x, c, r)
                     | x                     -> raise (ExpectedACons x))
                   | _           -> raise StackUnderflowExn)
      | 0x0a -> (match deref d with
                   | Cons(d', _) -> (d', c, r)
                   | _           -> raise StackUnderflowExn)
      | 0x0b -> (match deref d with
                   | Cons(r', _) -> (d, c, r')
                   | _           -> raise StackUnderflowExn)

      | 0x0c -> (match deref d with
                   | Cons(el, d') -> (match deref d' with
                     | Cons(th, d'') -> (match deref d'' with
                       | Cons(Int i, d''') ->
                         (d''', Cons((if i != 0 then th else el), c), r)
                       | x                 -> raise (ExpectedAnInt x))
                     | _             -> raise StackUnderflowExn)
                   | _            -> raise StackUnderflowExn)

      | 0x10 -> raise UnimplementedExn

      | _ -> raise (IllegalInsnExn insn)
