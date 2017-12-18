module rec PhiVal : sig
  type t =
    | Cons of t * t
    | Val of int * string
    | Ref of int * string

  val eq : t -> t -> bool
end = struct
  type t =
    | Cons of t * t
    | Val of int * string
    | Ref of int * string

  let eq = (=)
end

module Phi = struct
  open PhiVal

  let cons x y = Cons (x, y)
  let mkval s  = Val (Hashtbl.hash s, s)
  let nil      = mkval ""
  let mkref    = let ref_counter = ref 0 in
                 fun name ->
                   let ref_n = !ref_counter in
                   let () = ref_counter := ref_n + 1 in
                   Ref (ref_n, name)

  (* ocaml <-> phi *)
  exception NotAListExn of PhiVal.t
  exception NotAnIntExn of PhiVal.t

  let rec list_of_phi = function Cons (h, t) -> h :: list_of_phi t
                               | Val (0, "") -> []
                               | x           -> raise (NotAListExn x)
  let rec phi_of_list = function x :: xs -> cons x (phi_of_list xs)
                               | []      -> nil

  let int_of_phistring = function Val (_, x) -> (Char.code x.[0] lsl 24) lor
                                                (Char.code x.[1] lsl 16) lor
                                                (Char.code x.[2] lsl 8)  lor
                                                 Char.code x.[3]
                                | x          -> raise (NotAnIntExn x)
  let phistring_of_int x =
    let s = Bytes.create 4 in
    let () = Bytes.set s 0 (Char.chr ((x lsr 24) land 0xff)) in
    let () = Bytes.set s 1 (Char.chr ((x lsr 16) land 0xff)) in
    let () = Bytes.set s 2 (Char.chr ((x lsr 8 ) land 0xff)) in
    let () = Bytes.set s 3 (Char.chr ( x         land 0xff)) in
    mkval s

  (* type markers *)
  let iof_  = mkref "iof"
  let iof t = cons (cons iof_ t)

    (* primitive types *)
    let scope_t  = mkref "scope_t"
    let fn_t     = mkref "fn_t"

    let string_t = mkref "string_t"
    let int_t    = mkref "int_t"
    let symbol_t = mkref "symbol_t"
    let var_t    = mkref "var_t"
    let list_t   = mkref "list_t"

    (* generic parsers *)
    let parsealt_t = mkref "parsealt_t"
    let parseseq_t = mkref "parseseq_t"
    let parserep_t = mkref "parserep_t"

    (* string parsers *)
    let string_parsestate_t = mkref "string_parsestate_t"
    let strconst_t          = mkref "strconst_t"
    let strclass_t          = mkref "strclass_t"

    (* val parsers *)
    let val_parsestate_t = mkref "val_parsestate_t"
    let valcapture_t     = mkref "valcapture_t"
    let valmatch_t       = mkref "valmatch_t"
    let valtyped_t       = mkref "valtyped_t"

  (* operation markers *)
  let op_  = mkref "op"
  let op o = cons (cons op_ o)

    (* universal ops *)
    let typeof_op    = mkref "typeof_op"
    let stability_op = mkref "stability_op"
    let quote_op     = mkref "quote_op"     (* catalyzes introspection *)
    let unquote_op   = mkref "unquote_op"   (* inverts quote *)

    (* rewriting ops *)
    let parse_op   = mkref "parse_op"
    let rewrite_op = mkref "rewrite_op"

    (* int ops *)
    let int_plus_op  = mkref "int_plus_op"
    let int_times_op = mkref "int_times_op"
    let int_lt_op    = mkref "int_lt_op"
    let int_if_op    = mkref "int_if_op"
    let int_ord_op   = mkref "int_ord_op"

    (* string ops *)
    let string_chr_op    = mkref "string_chr_op"
    let string_cat_op    = mkref "string_cat_op"
    let string_length_op = mkref "string_length_op"
    let string_lt_op     = mkref "string_lt_op"

  (* TODO: base rewriting logic; I think this needs to be implemented in ocaml
     first, even if we intend to later rewrite in phi *)
end
