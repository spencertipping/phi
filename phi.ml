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
    | EvalOp
    | SymbolOp
    | VariableOp
    | RewriterOp
    | RewriteOp

    | LengthOp
    | SubstrOp
    | PlusOp
    | TimesOp
    | NegOp
    | CompareEqOp
    | CompareLtOp
    | AndOp

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
    | EvalOp
    | SymbolOp
    | VariableOp
    | RewriterOp
    | RewriteOp

    | LengthOp
    | SubstrOp
    | PlusOp
    | TimesOp
    | NegOp
    | CompareEqOp
    | CompareLtOp
    | AndOp

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

  let mknative name f = Native (name, f)
  let mkfn lhs rhs    = Cons (RewriterOp, Cons (lhs, rhs))
  let mksym name      = Cons (SymbolOp, mkstr name)
  let mkvar name      = Cons (VariableOp, mkstr name)

  let cons x y = Cons (x, y)
  let nil      = Int 0
  let quote x  = Cons (QuoteOp, x)

  let rec append x y = match x with
    | Int 0         -> y
    | Cons (xh, xt) -> Cons (xh, append xt y)
    | x             -> raise (PhiNotAListExn x)

  let rec list_to_phi = function
    | []      -> Int 0
    | x :: xs -> Cons (x, list_to_phi xs)

  let rec resolve = function
    | Forward (_, { contents = Some x }) -> resolve x
    | x                                  -> x

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
  let scope_apply scope v =
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

  let rec eval scope v = match scope_apply scope v with
    | Some x -> eval scope x
    | None   -> match scope_apply scope (Cons (EvalOp, v)) with
      | Some x -> eval scope x
      | None   -> v

  (* boot scope *)
  let bind name v = Cons (RewriterOp, Cons (mksym name, v))
  let boot_scope = list_to_phi [
    bind "quote_op"      QuoteOp;
    bind "eval_op"       EvalOp;
    bind "variable_op"   VariableOp;
    bind "rewriter_op"   RewriterOp;
    bind "rewrite_op"    RewriteOp;

    bind "length_op"     LengthOp;
    bind "substr_op"     SubstrOp;
    bind "plus_op"       PlusOp;
    bind "times_op"      TimesOp;
    bind "neg_op"        NegOp;
    bind "compare_eq_op" CompareEqOp;
    bind "compare_lt_op" CompareLtOp;
    bind "and_op"        AndOp;

    mknative "substr" (fun scope -> function
      | Cons (SubstrOp,
          Cons (Cons (QuoteOp, String (_, s)),
                Cons (Cons (QuoteOp, Int start),
                      Cons (QuoteOp, Int length)))) ->
             Some (quote (mkstr (String.sub s start length)))
      | _ -> None)

    mknative "length" (fun scope -> function
      | Cons (LengthOp, Cons (QuoteOp, String (_, s))) ->
          Some (quote (mkint (String.length s)))
      | _ -> None)

    mknative "plus" (fun scope -> function
      | Cons (PlusOp, (Cons (Cons (QuoteOp, Int a),
                             Cons (QuoteOp, Int b)))) ->
          Some (quote (mkint (a + b)))
      | _ -> None)
  ]
end
