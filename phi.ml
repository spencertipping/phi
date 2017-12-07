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
  let parse_continuation_op = mkobj "parse_continuation_op"
end
