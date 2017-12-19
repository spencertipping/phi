module rec PhiVal : sig
  type t =
    | Cons of t * t
    | Val  of int * string
    | Ref  of int * string

  val eq : t -> t -> bool
end = struct
  type t =
    | Cons of t * t
    | Val  of int * string
    | Ref  of int * string

  let eq = (=)
end

module Phi = struct
  open PhiVal

  let cons x y = Cons (x, y)
  let mkval s  = Val (Hashtbl.hash s, s)
  let mkref    = let ref_counter = ref 0 in
                 fun name ->
                   let ref_n = !ref_counter in
                   let () = ref_counter := ref_n + 1 in
                   Ref (ref_n, name)

  (* ocaml <-> phi *)
  exception NotAListExn of PhiVal.t
  exception NotAnIntExn of PhiVal.t

  let rec list_of_phi = function Cons (h, t) -> h :: list_of_phi t
                               | Nil         -> []
                               | x           -> raise (NotAListExn x)
  let rec phi_of_list = function x :: xs -> cons x (phi_of_list xs)
                               | []      -> Nil

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

  (* markers *)
  let iof t = cons (cons IOf t)
  let op  o = cons (cons Op  o)

  (* op implementations *)
end
