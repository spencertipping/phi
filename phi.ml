(* phi data structures *)
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

  val compare : t -> t -> int
  val equal   : t -> t -> bool
  val hash    : t      -> int
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

  let compare = compare
  let equal   = (=)
  let hash    = Hashtbl.hash
end

module PhiValHash : Hashtbl.S with type key = PhiVal.t = Hashtbl.Make(PhiVal)
module PhiValMap  : Map.S     with type key = PhiVal.t = Map.Make(PhiVal)
module PhiValSet  : Set.S     with type elt = PhiVal.t = Set.Make(PhiVal)

module Int = struct type t = int let compare = compare end
module IntMap : Map.S with type key = Int.t = Map.Make(Int)


(* data constructors *)
module PhiValUtils = struct
  open PhiVal

  let mksym name = Symbol (Hashtbl.hash name, name)
  let mkvar name = Variable (Hashtbl.hash name, name)
  let mkobj = let obj_id = ref 0 in
    fun name ->
      let n = !obj_id in
      let _ = obj_id := !obj_id + 1 in
      Object (n, name)

  let mkconstraint m t p = Constraint (Hashtbl.hash m, m, t, p)

  let mkmethod name = let h = Hashtbl.hash name in
    fun v -> Method (h, name, v)

  let forward_id = ref 0
  let forward _ =
    let n = !forward_id in
    let _ = forward_id := !forward_id + 1 in
    Forward (n, ref None)

  exception PhiNotAForwardExn of t
  let forward_set f v = match f with
    | Forward (n, f) -> f := Some v
    | _              -> raise (PhiNotAForwardExn f)

  let rec explain v = match v with
    | Nil               -> "nil"
    | Int n             -> string_of_int n
    | String s          -> "\"" ^ s ^ "\""
    | Cons (x, y)       -> "cons(" ^ explain x ^ ", " ^ explain y ^ ")"
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
end


module OptFns = struct
  let option_map f = function
    | Some x -> Some (f x)
    | None   -> None

  let option_flatmap f = function
    | Some x -> f x
    | None   -> None

  let is_some = function
    | Some _ -> true
    | None   -> false
end


(* generic + string parsers *)
module PhiParsers = struct
  open OptFns
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


(* phi-specific syntactic definitions *)
module PhiSyntax = struct
  open PhiVal
  open PhiValUtils
  open PhiParsers

  let string_of_chars cs =
    let buf = Buffer.create 16 in
    List.iter (Buffer.add_char buf) cs;
    Buffer.contents buf

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
                         (fun cs -> mksym (string_of_chars cs))

  let phi_string       = p_map (str "\"" ++ star (noneof "\\\"") ++ str "\"")
                         (fun ((_, x), _) -> String (string_of_chars x))

  let phi_digit        = oneof "0123456789"
  let phi_integer      = p_map (plus phi_digit)
                         (fun xs -> Int (int_of_string (string_of_chars xs)))

  let phi_parens x     = p_map (spaced (str "(") ++ x ++ spaced (str ")"))
                         (fun ((_, x), _) -> x)
end


(* phi root scope definitions *)
module PhiBoot = struct
  open OptFns
  open PhiVal
  open PhiValUtils
  open PhiParsers
  open PhiSyntax

  exception PhiMalformedListExn        of PhiVal.t
  exception PhiMalformedScopeExn       of PhiVal.t
  exception PhiMalformedParseResultExn of PhiVal.t
  exception PhiMalformedBindingsExn    of PhiVal.t
  exception PhiNotAConstraintExn       of PhiVal.t
  exception PhiNotCallableExn          of PhiVal.t

  let k x _ = x

  (* methods and constants *)
  let parse_continuation_ = mkmethod "parse_continuation"
  let with_               = mkmethod "with"

  let parse_state_ = mkobj "parse_state"

  let int_         = mkobj "int"
  and string_      = mkobj "string"
  and symbol_      = mkobj "symbol"
  and variable_    = mkobj "variable"
  and object_      = mkobj "object"
  and constraint_  = mkobj "constraint"
  and fn_          = mkobj "fn"
  and hosted_      = mkobj "hosted"
  and cons_        = mkobj "cons"
  and nil_         = mkobj "nil"
  and method_      = mkobj "method"
  and call_        = mkobj "call"

  let typed_ = mkconstraint "type"
  let type_  = mkmethod "type"
  let inc_   = mkmethod "inc"
  let size_  = mkmethod "size"

  let h_     = mkmethod "h"
  let t_     = mkmethod "t"

  (* phi calling convention *)
  let x_ = mkvar "x"
  let m_ = mkvar "m"
  let s_ = mkvar "s"

  let get_arg name =
    let h = Hashtbl.hash name in
    let rec get_arg' =
      function
        | Cons (Cons (Variable (i, _), x), xs) -> if h = i then x
                                                           else get_arg' xs
        | Nil          -> raise Not_found
        | Cons (x, xs) -> raise (PhiMalformedBindingsExn x)
        | x            -> raise (PhiMalformedListExn x) in
    get_arg'

  let with_arg a = let a_getter = get_arg a in
    fun f args -> f (a_getter args)

  let with_args args =
    let arg_list = Str.split (Str.regexp " +") args in
    let getters  = List.map get_arg arg_list in
    fun f args -> f (List.map (fun g -> g args) getters)

  (* parser interop *)
  let parser_of_x p = with_arg "x" (function
    | Cons (String s, Int n) -> (match p (s, n) with
      | Some (x, (s', n')) -> Some (Cons (x, Cons (String s', Int n')))
      | None               -> None)
    | _ -> None)

  (* term rewriting *)
  let rewrite v bs =
    let rec fill m = function
      | Cons (Cons (Variable (i, _), v), xs') -> fill (IntMap.add i v m) xs'
      | Nil                                   -> m
      | x                                     -> raise (PhiMalformedListExn x) in
    let m = fill IntMap.empty bs in
    let rec r x = match x with
      | Variable (i, _)         -> (try IntMap.find i m with Not_found -> x)
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

  (* eval *)
  let rec constraint_matches scope c v =
    is_constant v
    && match c with
       | Constraint (i, s, t, p) -> is_some (destructure scope t
                                               (eval scope (Method (i, s, v))))
       | _                       -> raise (PhiNotAConstraintExn c)

  and call scope = function
    | Cons _ as c   -> apply_scope_dynamic scope c
    | Fn     (l, r) -> fun i -> option_map     (rewrite r) (destructure scope l i)
    | Hosted (l, f) -> fun i -> option_flatmap f           (destructure scope l i)
    | x             -> raise (PhiNotCallableExn x)

  and apply_scope ps = apply_scope_dynamic ps ps

  and apply_scope_dynamic scope ps i =
    let rec apply' ps = match ps with
      | Nil           -> None
      | Cons (f, ps') -> (match call scope f i with
                            | None -> apply' ps'
                            | x    -> x)
      | _ -> raise (PhiMalformedListExn ps) in
    apply' ps

  and eval s v = match apply_scope s v with
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
  and destructure scope expr i =
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
          if constraint_matches scope c i then destructure scope p i
                                          else None
      | Method (he, _, ve),
        Method (hi, _, vi)   -> if he = hi then destructure scope ve vi
                                           else None
      | Call (ve, ae),
        Call (vi, ai)        -> twoparse scope ve ae vi ai
      | Cons (xe, ye),
        Cons (xi, yi)        -> twoparse scope xe ye xi yi
      | _                    -> None

  and twoparse scope e1 e2 i1 i2 = match destructure scope e1 i1,
                                         destructure scope e2 i2 with
    | Some re, Some ri -> Some (phi_list_append re ri)
    | _                -> None

  (* Reader *)
  let rec read_continuations s v i =
    let v = eval s v in
    match eval s (Call (parse_continuation_ v, s)) with
      | Call _ -> Cons (v, i)
      | sc     -> match apply_scope sc i with
        | None                -> Cons (v, i)
        | Some (Cons (vc, k)) -> let vn = eval s (Call (with_ v, vc)) in
                                 read_continuations s vn k
        | Some x              -> raise (PhiMalformedParseResultExn x)

  let read s i = match apply_scope s i with
    | None                -> None
    | Some (Cons (v, i')) -> Some (read_continuations s v i')
    | Some x              -> raise (PhiMalformedParseResultExn x)

  (* Boot scope *)
  let bind name v = Fn (mksym name, v)
  let boot_bindings =
    [ bind "int" int_;
      bind "string" string_;
      bind "symbol" symbol_;
      bind "variable" variable_;
      bind "object" object_;
      bind "fn" fn_;
      bind "hosted" hosted_;
      bind "nil" nil_;
      bind "cons" cons_;
      bind "constraint" constraint_;

      bind "method" method_;
      bind "call" call_;
      bind "parse_state" parse_state_ ]

  let int_literal    = spaced phi_integer
  let string_literal = spaced phi_string
  let symbol_literal = spaced phi_symbol

  let method_k       = spaced (p_map (str "." ++ phi_symbol)
                              (fun (_, x) -> Cons (method_, x)))

  (* method parse continuation *)
  let method_parse_continuation =
    [ Hosted (Call (parse_continuation_ x_, s_),
              k (Some (Cons (Hosted (x_, parser_of_x method_k), Nil))));

      Hosted (Call (with_ x_, Cons (method_, m_)),
              with_args "m x" (function
                | [Symbol (h, s); v] -> Some (Method (h, s, v))
                | _                  -> None)) ]

  let rec typeof = function
    | Int _        -> Some int_
    | String _     -> Some string_
    | Symbol _     -> Some symbol_
    | Variable _   -> Some variable_
    | Object _     -> Some object_
    | Fn _         -> Some fn_
    | Hosted _     -> Some hosted_
    | Nil          -> Some nil_
    | Cons _       -> Some cons_
    | Constraint _ -> Some constraint_

    | Forward (_, { contents = Some v }) -> typeof v
    | _                                  -> None

  let int_fn f = with_arg "x" (function
    | Int n -> f n
    | _     -> None)

  let str_fn f = with_arg "x" (function
    | String s -> f s
    | _        -> None)

  let boot_scope_ref = ref Nil
  let boot_scope = phi_of_list (
    phi_of_list boot_bindings ::
    phi_of_list method_parse_continuation ::
    [
      Hosted (mksym "phi_root", fun args -> Some !boot_scope_ref);

      Hosted (type_ x_, with_arg "x" typeof);

      Hosted (h_ (typed_ cons_ x_),
              with_arg "x" (function Cons (h, t) -> Some h
                                   | _           -> None));

      Hosted (t_ (typed_ cons_ x_),
              with_arg "x" (function Cons (h, t) -> Some t
                                   | _           -> None));

      Hosted (t_ (typed_ nil_ x_), with_arg "x" (function _ -> Some Nil));

      Hosted (inc_ (typed_ int_ x_),
              int_fn (fun x -> Some (Int (x + 1))));

      Hosted (size_ (typed_ string_ x_),
              str_fn (fun x -> Some (Int (String.length x))));
    ]
    @
    List.map (fun p -> Hosted (Cons (parse_state_, x_), parser_of_x p))
      [ int_literal;
        string_literal;
        symbol_literal ])

  let () = boot_scope_ref := boot_scope
end

open PhiVal
open PhiValUtils
open PhiParsers
open PhiSyntax
open PhiBoot

let typed_explain s v =
  let t = eval s (type_ v) in
  explain v ^ " : " ^ explain t

let rec repl () =
  try let s  = input_line stdin in
      let st = Unix.gettimeofday () in
      let p  = read boot_scope (Cons (parse_state_, Cons (String s, Int 0))) in
      let p' = match p with
        | Some (Cons (x, _)) -> eval boot_scope x
        | _                  -> String ("failed to parse " ^ s) in
      let et = Unix.gettimeofday () in
      let ex = match p with
        | Some (Cons (_, Cons (String s, Int n))) -> String.sub s 0 n
        | _ -> "" in
      let () = print_string (ex ^ "\n= " ^ typed_explain boot_scope p' ^ "\n") in
      let () = print_string ("in " ^ string_of_float ((et -. st) *. 1000.)
                                   ^ "ms\n\n") in
      repl ()
  with End_of_file -> ()

let () = repl ()
