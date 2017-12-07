(* phi bootstrap interpreter *)
module rec PhiVal : sig
  type t =
    | Int     of int
    | String  of int * string
    | Object  of int * string
    | Cons    of t * t
    | Forward of int * t option ref
    | Native  of string * (t -> t -> t option)

    (* objects written as cases to facilitate ocaml pattern matching *)
    | QuoteOp
    | VariableOp
    | RewriterOp

  val equal : t -> t -> bool
end = struct
  type t =
    | Int     of int
    | String  of int * string
    | Object  of int * string
    | Cons    of t * t
    | Forward of int * t option ref
    | Native  of string * (t -> t -> t option)

    | QuoteOp
    | VariableOp
    | RewriterOp

  let equal = (=)
end

module Phi = struct
  open PhiVal

  exception PhiNotAListExn        of t
  exception PhiNotABindingListExn of t
  exception PhiNotAScopeListExn   of t

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
    | Cons (xh, xt) -> Cons (xh, append xt y)
    | Int 0         -> y
    | x             -> raise (PhiNotAListExn x)

  let rec resolve = function
    | Forward (_, { contents = Some x }) -> resolve x
    | x                                  -> x

  (* base evaluation protocol *)
  let call_op   = mkobj "call_op"
  let method_op = mkobj "method_op"
  let eval_op   = mkobj "eval_op"
  let symbol_op = mkobj "symbol_op"

  (* term rewriting protocol *)
  let rewrite_op  = mkobj "rewrite_op"
  let match_op    = mkobj "match_op"

  (* parse continuation protocol *)
  let begin_atom_op         = mkobj "begin_atom_op"
  let parse_continuation_op = mkobj "parse_continuation_op"

  (* term rewriting logic *)
  let rec destructure x y = match resolve x, resolve y with
    | Cons (VariableOp, String _) as vc, _ -> Some (Cons (Cons (vc, y), nil))
    | Cons (a, b), Cons (c, d) ->
        (match destructure a c with
           | None     -> None
           | Some vac -> match destructure b d with
             | None     -> None
             | Some vbd -> Some (append vac vbd))
    | x, y -> if x = y then Some nil else None

  let rec rewrite bindings x =
    let rec rewrite' = function
      | Int 0 -> x
      | Cons (Cons (Cons (VariableOp, String (h, _)), v), bs') ->
          (match x with
            | Cons (VariableOp, String (xh, _)) -> if h = xh then v
                                                             else rewrite' bs'
            | Cons (x, y)                       -> Cons (rewrite bindings x,
                                                         rewrite bindings y)
            | _                                 -> x)
      | x -> raise (PhiNotABindingListExn x) in
    rewrite' bindings

  (* scope application logic *)
  let rec scope_apply scope v =
    let rec scope_apply' = function
      | Int 0 -> None
      | Cons (Native (_, f), s') ->
          (match f scope v with
            | None        -> scope_apply' s'
            | Some _ as x -> x)
      | Cons (Cons (RewriterOp, Cons (lhs, rhs)), s') ->
          (match destructure lhs v with
            | Some bs -> Some (rewrite bs rhs)
            | None    -> scope_apply' s')
      | x -> raise (PhiNotAScopeListExn x) in
    scope_apply' scope
end
