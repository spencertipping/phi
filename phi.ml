module rec PhiVal : sig
  type t =
  | Nil
  | Int     of int
  | String  of string
  | Cons    of t * t
  | Binding of int * string * t
  | Symbol  of int * string
  | Forward of t ref
  | Method  of int * string * t
  | Call    of t * t
  | Fn      of t * t

  val (%.)  : t -> string -> t
  val (%@)  : t -> t -> t
  val (%::) : t -> t -> t

  val explain : t -> string

  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val hash    : t      -> int
end = struct
  type t =
  | Nil
  | Int     of int
  | String  of string
  | Cons    of t * t
  | Binding of int * string * t
  | Symbol  of int * string
  | Forward of t ref
  | Method  of int * string * t
  | Call    of t * t
  | Fn      of t * t

  let (%.)  v m = Method (Hashtbl.hash m, m, v)
  let (%@)  u v = Call (u, v)
  let (%::) u v = Cons (u, v)

  let rec explain v = match v with
    | Nil               -> "nil"
    | Int    n          -> string_of_int n
    | String s          -> "\"" ^ s ^ "\""
    | Cons (x, y)       -> explain x ^ " :: " ^ explain y
    | Binding (_, s, v) -> s ^ " = " ^ explain v
    | Symbol (_, s)     -> s
    | Forward x         -> "forward[" ^ explain !x ^ "]"
    | Method (_, s, v)  -> "(" ^ explain v ^ ")." ^ s
    | Call (v, a)       -> "(" ^ explain v ^ ")(" ^ explain a ^ ")"
    | Fn (a, v)         -> "(" ^ explain a ^ ") -> (" ^ explain v ^ ")"

  let compare = compare
  let equal   = (=)
  let hash    = Hashtbl.hash
end

and PhiValHash : Hashtbl.S with type key = PhiVal.t = Hashtbl.Make(PhiVal)
and PhiValMap  : Map.S     with type key = PhiVal.t = Map.Make(PhiVal)
and PhiValSet  : Set.S     with type elt = PhiVal.t = Set.Make(PhiVal)

module OhFFS = struct
  type t = int
  let compare = compare
end

module BindingMap : Map.S with type key = OhFFS.t = Map.Make(OhFFS)

module PhiParsers = struct
  open PhiVal

  (* String parsers *)
  let str s (v, i) =
    if i + (String.length s) < String.length v
    && s = String.sub v i (String.length s) then Some (s, (v, i + String.length s))
    else                                         None

  let chartest b cs (v, i) =
    if i < String.length v && b = String.contains cs v.[i] then
      Some (v.[i], (v, i + 1))
    else None

  let oneof  = chartest true
  let noneof = chartest false

  (* General *)
  let empty i = Some ((), i)
  let none  i = None

  (* High-order parsers *)
  let seq f g i = match f i with
    | None          -> None
    | Some (r1, i') -> match g i' with
      | None           -> None
      | Some (r2, i'') -> Some ((r1, r2), i'')
  let (++) = seq

  let either f g i = match f i with
    | None -> g i
    | x    -> x

  let rec any xs i = match xs with
    | []       -> None
    | x :: xs' -> match x i with
      | None -> any xs' i
      | r    -> r

  let rep p min i =
    let rec rep' min i rs =
      match p i with
        | Some (r, i') -> rep' (min-1) i' (r :: rs)
        | None         -> if min <= 0 then Some (List.rev rs, i)
                          else             None in
    rep' min i []

  let star p = rep p 0
  let plus p = rep p 1

  let p_mbind p f i = match p i with
    | Some (v, i') -> f v i'
    | None         -> None

  let p_map p f i = match p i with
    | Some (v, i') -> Some (f v, i')
    | None         -> None
end

module PhiSyntax = struct
  open PhiVal
  open PhiParsers

  exception PhiMalformedListExn of PhiVal.t

  let string_of_chars cs =
    let buf = Buffer.create 16 in
    List.iter (Buffer.add_char buf) cs;
    Buffer.contents buf

  let make_symbol s    = Symbol (Hashtbl.hash s, s)

  let phi_line_comment = p_map (str "#" ++ star (noneof "\n")) (fun _ -> ())
  let phi_whitespace   = p_map (plus (oneof " \t\r\n"))        (fun _ -> ())
  let phi_ignore       = either phi_whitespace phi_line_comment

  let spaced x         = p_map (star phi_ignore ++ x ++ star phi_ignore)
                         (fun ((_, x), _) -> x)

  let phi_symbol_char  = oneof ("abcdefghijklmnopqrstuvwvyz_"
                              ^ "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  let phi_symbol       = p_map (plus phi_symbol_char)
                         (fun cs -> make_symbol (string_of_chars cs))

  let phi_string       = p_map (str "\"" ++ star (noneof "\\\"") ++ str "\"")
                         (fun ((_, x), _) -> String (string_of_chars x))

  let phi_digit        = oneof "0123456789"
  let phi_integer      = p_map (plus phi_digit)
                         (fun xs -> Int (int_of_string (string_of_chars xs)))

  let phi_parens x     = p_map (spaced (str "(") ++ x ++ spaced (str ")"))
                         (fun ((_, x), _) -> x)

  let phi_lift_sparser p i = match i with
    | Cons (String s, Int n) -> (match p (s, n) with
      | Some (x, (s', n')) -> Some (Cons (x, Cons (String s', Int n')))
      | None               -> None)
    | _ -> None
end

module PhiBoot = struct
  open PhiVal
  open PhiParsers
  open PhiSyntax

  exception PhiMalformedScopeExn       of PhiVal.t
  exception PhiMalformedParseResultExn of PhiVal.t

  let phi_rewrite v bs =
    let rec fill m = function
      | Cons (Binding (i, _, v), xs') -> fill (BindingMap.add i v m) xs'
      | Nil                           -> m
      | x                             -> raise (PhiMalformedListExn x) in
    let m = fill BindingMap.empty bs in
    let rec r x = match x with
      | Symbol (i, _)     -> (try BindingMap.find i m with Not_found -> x)
      | Method (i, s, v)  -> Method (i, s, r v)
      | Call (v, a)       -> Call (r v, r a)
      | Cons (x, y)       -> Cons (r x, r y)
      | Binding (i, x, v) -> Binding (i, x, r v)
      | Fn (a, v)         -> Fn (r a, r v)
      | x                 -> x in
    r v

  let rec phi_any ps i = match ps, i with
    | Nil, _ -> None
    | Cons (Binding (bs, bi, v), ps'), Symbol (is, ii) ->
        if bi = ii then Some v else phi_any ps' i
    | Cons (Fn (l, r), ps'), _ ->
        (match phi_lift_vparser l i with
           | Some ls -> Some (phi_rewrite r ls)
           | None    -> phi_any ps' i)
    | Cons (x, _), _ -> raise (PhiMalformedScopeExn x)
    | _              -> raise (PhiMalformedListExn ps)

  and eval s v = match phi_any s v with
    | Some v' -> eval s v'
    | None    -> v

  and resolve v = match v with
    | Forward x -> resolve !x
    | _         -> v

  and phi_list_append xs ys = match xs with
    | Cons (x, xs') -> phi_list_append xs' (Cons (x, ys))
    | Nil           -> ys
    | _             -> raise (PhiMalformedListExn xs)

  and phi_of_list xs = match xs with
    | x :: xs' -> Cons (x, phi_of_list xs')
    | []       -> Nil

  and phi_lift_vparser expr i =
    match expr, resolve i with
      | Int ne,    Int ni    -> if ne = ni then Some Nil else None
      | String se, String si -> if se = si then Some Nil else None
      | Symbol (i, s), x     -> Some (Binding (i, s, x))
      | Method (he, _, ve),
        Method (hi, _, vi)   -> if he = hi then phi_lift_vparser ve vi else None
      | Call (ve, ae),
        Call (vi, ai)        -> twoparse ve ae vi ai
      | Cons (xe, ye),
        Cons (xi, yi)        -> twoparse xe ye xi yi
      | _                    -> None

  and twoparse e1 e2 i1 i2 = match phi_lift_vparser e1 i1,
                                   phi_lift_vparser e2 i2 with
    | Some re, Some ri -> Some (phi_list_append re ri)
    | _                -> None

  (* Reader *)
  let rec read_continuations s v i =
    match eval s (Call (v %. "#parse_continuation", s)) with
      | Call _ -> Cons (v, i)
      | sc     -> match phi_any sc i with
        | None                -> Cons (v, i)
        | Some (Cons (vc, k)) ->
            let vn = eval s (Call (v %. "#with_continuation", vc)) in
            read_continuations s vn k
        | Some x -> raise (PhiMalformedParseResultExn x)

  let read s i = match phi_any s i with
    | None                -> None
    | Some (Cons (v, i')) -> Some (read_continuations s v i')
    | Some x              -> raise (PhiMalformedParseResultExn x)

  (* Boot scope *)
  let int_literal    = spaced phi_integer
  let string_literal = spaced phi_string
  let symbol_literal = spaced phi_symbol
  let method_k       = spaced (p_map (str "." ++ phi_symbol) (fun (_, x) -> x))

  let boot_scope = phi_of_list (
    List.map (fun x -> Fn (phi_lift_sparser x)) [
      (* TODO: port this to the world of functions, where everything gets
         applied all the time. We need a scope.#parse(string, n) method binding
         I think. *)
      int_literal;
      string_literal;
      symbol_literal]
    @
    List.map (fun x -> Fn (phi_lift_vparser x)) [
      (* TODO: figure this out *)
      ])
end

open PhiVal
open PhiParsers
open PhiSyntax
open PhiBoot

let typed_explain s v =
  let t = eval s (v %. "#type") in
  explain v ^ " : " ^ explain t

let rec repl () =
  try let () = print_string "> "; flush stdout in
      let s  = input_line stdin in
      let p  = read boot_scope (Cons (String s, Int 0)) in
      let () = match p with
        | Some (Cons (x, Cons (_, Int n)))
                 -> print_string ("= " ^ typed_explain boot_scope x ^ "\n")
        | Some x -> print_string ("= " ^ typed_explain boot_scope x ^ "\n")
        | None   -> print_string ("failed to parse " ^ s ^ "\n") in
      repl ()
  with End_of_file -> ()

let () = repl ()
