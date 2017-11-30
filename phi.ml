module rec PhiVal : sig
  type t =
  | Nil
  | Int        of int
  | String     of string
  | Cons       of t * t
  | Symbol     of int * string
  | Variable   of int * string
  | Forward    of int * t option ref
  | Object     of int * string
  | Method     of int * string * t
  | Constraint of int * string * t * t
  | Call       of t * t
  | Fn         of t * t
  | Hosted     of t * (t -> t option)

  val (%.)  : t -> string -> t
  val (%@)  : t -> t -> t
  val (%::) : t -> t -> t

  val explain : t -> string

  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val hash    : t      -> int

  val forward : t -> t
  val obj     : string -> t

  exception PhiNotAForwardExn of t
end = struct
  type t =
  | Nil
  | Int        of int
  | String     of string
  | Cons       of t * t
  | Symbol     of int * string
  | Variable   of int * string
  | Forward    of int * t option ref
  | Object     of int * string
  | Method     of int * string * t
  | Constraint of int * string * t * t
  | Call       of t * t
  | Fn         of t * t
  | Hosted     of t * (t -> t option)

  let (%.)  v m = Method (Hashtbl.hash m, m, v)
  let (%@)  u v = Call (u, v)
  let (%::) u v = Cons (u, v)

  let rec explain v = match v with
    | Nil               -> "nil"
    | Int    n          -> string_of_int n
    | String s          -> "\"" ^ s ^ "\""
    | Cons (x, y)       -> explain x ^ " :: " ^ explain y
    | Symbol (_, s)     -> s
    | Variable (_, s)   -> "$" ^ s
    | Object (n, s)     -> "#" ^ string_of_int n ^ "(" ^ s ^ ")"
    | Forward (n, x)    -> "forward " ^ string_of_int n ^ (match !x with
                             | Some v -> "[" ^ explain v ^ "]"
                             | None   -> "")
    | Method (_, s, v)  -> explain v ^ "." ^ s
    | Constraint (_, s, t, v) -> explain v ^ ":(." ^ s ^ " ~ " ^ explain t ^ ")"
    | Call (v, a)       -> "(" ^ explain v ^ ")(" ^ explain a ^ ")"
    | Fn (a, v)         -> "(" ^ explain a ^ ") -> (" ^ explain v ^ ")"
    | Hosted (p, f)     -> "hosted(" ^ explain p ^ ")"

  let compare = compare
  let equal   = (=)
  let hash    = Hashtbl.hash

  let forward_id = ref 0
  let forward _ =
    let n = !forward_id in
    let _ = forward_id := !forward_id + 1 in
    Forward (n, ref None)

  exception PhiNotAForwardExn of t
  let forward_set f v = match f with
    | Forward (n, f) -> f := Some v
    | _              -> raise (PhiNotAForwardExn f)

  let obj_id = ref 0
  let obj name =
    let n = !obj_id in
    let _ = obj_id := !obj_id + 1 in
    Object (n, name)
end

and PhiValHash : Hashtbl.S with type key = PhiVal.t = Hashtbl.Make(PhiVal)
and PhiValMap  : Map.S     with type key = PhiVal.t = Map.Make(PhiVal)
and PhiValSet  : Set.S     with type elt = PhiVal.t = Set.Make(PhiVal)

module Int = struct type t = int let compare = compare end
module IntMap : Map.S with type key = Int.t = Map.Make(Int)

module PhiParsers = struct
  open PhiVal

  (* String parsers *)
  let str s (v, i) =
    if i + String.length s <= String.length v
    && s = String.sub v i (String.length s) then Some (s, (v, i + String.length s))
    else                                         None

  let chartest b cs (v, i) =
    if i < String.length v && b = String.contains cs v.[i] then
      Some (v.[i], (v, i + 1))
    else None

  let option_map f = function
    | Some x -> Some (f x)
    | None   -> None

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

  let rep min p i =
    let rec rep' min i rs =
      match p i with
        | Some (r, i') -> rep' (min-1) i' (r :: rs)
        | None         -> if min <= 0 then Some (List.rev rs, i)
                          else             None in
    rep' min i []

  let star = rep 0
  let plus = rep 1

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

  (* NB: I'm not sure why we need to have phi_ignore return a char instead of a
     unit; I'm chalking this up to some type system bug for now, hence the
     otherwise inexplicable workaround. *)
  let phi_line_comment = p_map (str "#" ++ star (noneof "\n")) (fun _ -> '#')
  let phi_whitespace   = p_map (plus (oneof " \t\r\n"))        (fun _ -> ' ')
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
      | Cons (Cons (Variable (i, _), v), xs') -> fill (IntMap.add i v m) xs'
      | Nil                                   -> m
      | x                                     -> raise (PhiMalformedListExn x) in
    let m = fill IntMap.empty bs in
    let rec r x = match x with
      | Symbol (i, _)           -> (try IntMap.find i m with Not_found -> x)
      | Method (i, s, v)        -> Method (i, s, r v)
      | Call (v, a)             -> Call (r v, r a)
      | Cons (x, y)             -> Cons (r x, r y)
      | Fn (a, v)               -> Fn (r a, r v)
      | Constraint (i, s, t, v) -> Constraint (i, s, r t, r v)
      | Forward (i,
         { contents = Some x }) -> Forward (i, ref (Some (r x)))
      | x                       -> x in
    r v

  let is_constant = function
    | Method _ -> false
    | Call _   -> false
    | _        -> true

  exception PhiNotAConstraintExn of PhiVal.t
  let rec constraint_matches scope c v = match c with
    | Constraint (i, s, t, p) ->
        let tv = eval scope (Method (i, s, v)) in
        (match phi_lift_vparser scope t tv with
           | Some _ -> true
           | None   -> false)
    | _ -> raise (PhiNotAConstraintExn c)

  and phi_any ps i =
    let scope = ps in
    let rec phi_any' ps = match ps with
      | Nil -> None
      | Cons (Fn (l, r), ps') ->
          (match phi_lift_vparser scope l i with
             | Some ls -> Some (phi_rewrite r ls)
             | None    -> phi_any' ps')
      | Cons (Hosted (l, f), ps') ->
          (match phi_lift_vparser scope l i with
             | None    -> phi_any' ps'
             | Some bs -> match f bs with
               | None -> phi_any' ps'
               | x    -> x)
      | Cons (x, _) -> raise (PhiMalformedScopeExn x)
      | _           -> raise (PhiMalformedListExn ps) in
    phi_any' ps

  and eval s v = match phi_any s v with
    | Some v' -> eval s v'
    | None    -> v

  and resolve v = match v with
    | Forward (n, { contents = Some x }) -> resolve x
    | _                                  -> v

  and phi_list_append xs ys = match xs with
    | Cons (x, xs') -> phi_list_append xs' (Cons (x, ys))
    | Nil           -> ys
    | _             -> raise (PhiMalformedListExn xs)

  and phi_of_list xs = match xs with
    | x :: xs' -> Cons (x, phi_of_list xs')
    | []       -> Nil

  (* NB: this can be converted into a meta scope that governs how pattern
     matching works *)
  and phi_lift_vparser scope expr i =
    match expr, resolve i with
      | Variable _ as v, i   -> Some (Cons (Cons (v, i), Nil))
      | Int ne,    Int ni    -> if ne = ni then Some Nil else None
      | String se, String si -> if se = si then Some Nil else None
      | Symbol (i1, s1),
        Symbol (i2, s2)      -> if i1 = i2 then Some Nil else None
      | Object (a, _),
        Object (b, _)        -> if a = b then Some Nil else None
      | Constraint (_,_,_,p) as c,
        i                    ->
          if is_constant i
          && constraint_matches scope c i then phi_lift_vparser scope p i
                                          else None
      | Method (he, _, ve),
        Method (hi, _, vi)   -> if he = hi then phi_lift_vparser scope ve vi
                                           else None
      | Call (ve, ae),
        Call (vi, ai)        -> twoparse scope ve ae vi ai
      | Cons (xe, ye),
        Cons (xi, yi)        -> twoparse scope xe ye xi yi
      | _                    -> None

  and twoparse scope e1 e2 i1 i2 = match phi_lift_vparser scope e1 i1,
                                         phi_lift_vparser scope e2 i2 with
    | Some re, Some ri -> Some (phi_list_append re ri)
    | _                -> None

  (* Reader *)
  let rec read_continuations s v i =
    match eval s (Call (v %. "parse_continuation", s)) with
      | Call _ -> Cons (v, i)
      | sc     -> match phi_any sc i with
        | None                -> Cons (v, i)
        | Some (Cons (vc, k)) -> let vn = eval s (Call (v %. "with", vc)) in
                                 read_continuations s vn k
        | Some x              -> raise (PhiMalformedParseResultExn x)

  let read s i = match phi_any s i with
    | None                -> None
    | Some (Cons (v, i')) -> Some (read_continuations s v i')
    | Some x              -> raise (PhiMalformedParseResultExn x)

  (* Boot scope *)
  let int_type        = obj "int"
  let string_type     = obj "string"
  let symbol_type     = obj "symbol"
  let variable_type   = obj "variable"
  let object_type     = obj "object"
  let constraint_type = obj "constraint"
  let fn_type         = obj "fn"
  let hosted_type     = obj "hosted"
  let cons_type       = obj "cons"
  let nil_type        = obj "nil"

  let method_type     = obj "method"
  let call_type       = obj "call"

  let int_literal    = spaced phi_integer
  let string_literal = spaced phi_string
  let symbol_literal = spaced phi_symbol
  let method_k       = spaced (p_map (str "." ++ phi_symbol)
                              (fun (_, x) -> Cons (method_type, x)))

  (* TODO: do we define custom types by binding detailed rewrite rules for a
     scope? Like using the head of a cons cell if it's a custom type:

     (cons point (cons x y)).type == point
  *)
  let rec typeof = function
    | Int _        -> Some int_type
    | String _     -> Some string_type
    | Symbol _     -> Some symbol_type
    | Variable _   -> Some variable_type
    | Object _     -> Some object_type
    | Fn _         -> Some fn_type
    | Hosted _     -> Some hosted_type
    | Nil          -> Some nil_type
    | Cons _       -> Some cons_type
    | Constraint _ -> Some constraint_type

    | Forward (_, { contents = Some v }) -> typeof v
    | _                                  -> None

  let method_wrap m f = let mh = Hashtbl.hash m in
    function
      | Method (h, _, v) -> if h = mh then f v else None
      | _                -> None

  let bind name v = Fn (Symbol (Hashtbl.hash name, name), v)

  let typed t f = Constraint (Hashtbl.hash "type", "type", t, f)

  (* Calling convention conversions *)
  let arg name = Variable (Hashtbl.hash name, name)
  let get_arg name =
    let h = Hashtbl.hash name in
    let rec get_arg' =
      function
        | Cons (Cons (Variable (i, _), x), xs) -> if h = i then x
                                                           else get_arg' xs
        | Cons (x, xs) -> raise (PhiMalformedListExn x)
        | Nil          -> raise Not_found
        | x            -> raise (PhiMalformedListExn x) in
    get_arg'

  let boot_scope_ref = ref Nil
  let boot_scope = phi_of_list (
    [
      bind "int" int_type;
      bind "string" string_type;
      bind "symbol" symbol_type;
      bind "variable" variable_type;
      bind "object" object_type;
      bind "fn" fn_type;
      bind "hosted" hosted_type;
      bind "nil" nil_type;
      bind "cons" cons_type;
      bind "constraint" constraint_type;

      bind "method" method_type;
      bind "call" call_type;

      Hosted (Symbol (Hashtbl.hash "phi_root", "phi_root"),
              fun args -> Some !boot_scope_ref);

      Hosted (arg "x" %. "type", fun args -> typeof (get_arg "x" args));

      (* TODO: handle parse continuations here, e.g. for methods *)

      Hosted (arg "x" %. "parse_continuation" %@ arg "s",
              fun _ ->
                Some (Cons (
                  Hosted (arg "x",
                          fun args -> phi_lift_sparser method_k (get_arg "x" args)),
                  Nil)));

      Hosted (arg "x" %. "with" %@ Cons (method_type, arg "m"),
              fun args -> match get_arg "m" args with
                | Symbol (h, s) -> Some (Method (h, s, get_arg "x" args))
                | _             -> None);

      Hosted (typed int_type (arg "x") %. "inc",
              fun args -> match get_arg "x" args with
                | Int n -> Some (Int (n + 1))
                | _     -> None);

      Hosted (typed string_type (arg "x") %. "size",
              fun args -> match get_arg "x" args with
                | String s -> Some (Int (String.length s))
                | _        -> None)
    ]
    @
    List.map (fun x -> Hosted (arg "x" %. "#parse_state",
                               fun args -> phi_lift_sparser x (get_arg "x" args)))
      [ int_literal;
        string_literal;
        symbol_literal ])

  let () = boot_scope_ref := boot_scope
end

open PhiVal
open PhiParsers
open PhiSyntax
open PhiBoot

let typed_explain s v =
  let t = eval s (v %. "type") in
  explain v ^ " : " ^ explain t

let rec repl () =
  try let () = print_string "> "; flush stdout in
      let s  = input_line stdin in
      let st = Unix.gettimeofday () in
      let p  = read boot_scope (Cons (String s, Int 0) %. "#parse_state") in
      let p' = match p with
        | Some (Cons (x, _)) -> eval boot_scope x
        | _                  -> String "failed to parse" in
      let et = Unix.gettimeofday () in
      let () = print_string ("= " ^ typed_explain boot_scope p' ^ "\n") in
      let () = print_string ("in " ^ string_of_float ((et -. st) *. 1000.)
                                   ^ "ms\n") in
      repl ()
  with End_of_file -> ()

let () = repl ()
