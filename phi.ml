(* phi bootstrap interpreter *)
module rec PhiVal : sig
  type t =
    | Int     of int
    | String  of int * string
    | Object  of int * string
    | Cons    of t * t
    | Forward of int * t option ref

  val equal : t -> t -> bool
end = struct
  type t =
    | Int     of int
    | String  of int * string
    | Object  of int * string
    | Cons    of t * t
    | Forward of int * t option ref

  let equal = (=)
end

module Phi = struct
  open PhiVal

  exception PhiNotAListExn of t

  (* type constructors *)
  let mkint i = Int i
  let mkstr s = String (Hashtbl.hash s, s)
  let mkobj =
    let obj_counter = ref 0 in
    fun name -> let () = obj_counter := !obj_counter + 1 in
                Object (!obj_counter, name)

  let mkfwd =
    let fwd_counter = ref 0 in
    fun () -> let () = fwd_counter := !fwd_counter + 1 in
              Forward (!fwd_counter, ref None)

  let cons x y = Cons (x, y)
  let nil      = Int 0

  let rec append x y = match x with
    | Cons (xh, xt) -> append xt (Cons (xh, y))
    | Int 0         -> y
    | x             -> raise (PhiNotAListExn x)

  let rec resolve = function
    | Forward (_, { contents = Some x }) -> resolve x
    | x                                  -> x

  (* base evaluation protocol *)
  let quote_op    = mkobj "quote_op"
  let call_op     = mkobj "call_op"
  let method_op   = mkobj "method_op"
  let eval_op     = mkobj "eval_op"
  let symbol_op   = mkobj "symbol_op"
  let variable_op = mkobj "variable_op"

  (* term rewriting protocol *)
  let rewriter_op = mkobj "rewriter_op"
  let rewrite_op  = mkobj "rewrite_op"
  let match_op    = mkobj "match_op"

  (* parse continuation protocol *)
  let begin_atom_op         = mkobj "begin_atom_op"
  let parse_continuation_op = mkobj "parse_continuation_op"

  (* term rewriting logic *)
  let rec destructure x y = match resolve x, resolve y with
    | Cons (variable_op, _) as vc, _ -> Some (Cons (Cons (vc, y), nil))
    | Cons (a, b), Cons (c, d)       ->
        (match destructure a c with
           | None     -> None
           | Some vac -> match destructure b d with
             | None     -> None
             | Some vbd -> Some (append vac vbd))
    | Int a,          Int b          -> if a  = b  then Some nil else None
    | String (ha, _), String (hb, _) -> if ha = hb then Some nil else None
    | _                              -> None
end
