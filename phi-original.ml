(* phi data structures *)
module rec PhiVal : sig
  type t =
  | Nil
  | Int        of int
  | String     of string
  | Cons       of t * t             (* Q: can we make cons a "quoted call"? *)
  | Symbol     of int * string
  | Variable   of int * string
  | Forward    of int * t option ref
  | Object     of int * string
  | Method     of int * string * t  (* Q: is there a "quoted method"? *)
                                    (* (of course, what's the point?) *)
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


module PhiValUtils = struct
  open PhiVal

  exception UhOh of t

  exception PhiMalformedListExn        of t
  exception PhiMalformedScopeExn       of t
  exception PhiMalformedParseResultExn of t
  exception PhiMalformedBindingsExn    of t
  exception PhiNotAConstraintExn       of t
  exception PhiNotCallableExn          of t
  exception PhiNotAForwardExn          of t
  exception PhiNotAScopeTypeExn        of t

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

  let mkforward = let forward_id = ref 0 in
    fun () ->
      let n = !forward_id in
      let _ = forward_id := !forward_id + 1 in
      Forward (n, ref None)

  let forward_set f v = match f with
    | Forward (n, f) -> f := Some v
    | _              -> raise (PhiNotAForwardExn f)

  let rec is_proper_list = function
    | Nil          -> true
    | Cons (_, xs) -> is_proper_list xs
    | _            -> false

  let rec explain_proper_list = function
    | Cons (x, Nil)           -> explain x
    | Cons (x, (Cons _ as c)) -> explain x ^ ", " ^ explain_proper_list c
    | Nil                     -> ""
    | x                       -> raise (UhOh x)

  and explain v = match v with
    | Nil               -> "nil"
    | Int n             -> string_of_int n
    | String s          -> "\"" ^ s ^ "\""
    | Cons (x, y) as c  ->
        if is_proper_list c
          then "[" ^ explain_proper_list c ^ "]"
          else "cons(" ^ explain x ^ ", " ^ explain y ^ ")"
    | Symbol (_, s)     -> s
    | Variable (_, s)   -> "$" ^ s
    | Object (n, s)     -> "#" ^ string_of_int n ^ "(" ^ s ^ ")"
    | Forward (n, x)    -> "forward " ^ string_of_int n ^ (match !x with
                             | Some _ -> ""
                             | None   -> "unset")
    | Method (_, s, v)  -> explain v ^ "." ^ s
    | Constraint (_, s, t, v) -> explain v ^ ":(." ^ s ^ " ~ " ^ explain t ^ ")"
    | Call (v, a)       -> "(" ^ explain v ^ ")(" ^ explain a ^ ")"
    | Fn (a, v)         -> "(" ^ explain a ^ ") -> (" ^ explain v ^ ")"
    | Hosted (p, f)     -> "hosted(" ^ explain p ^ ")"

  let rec phi_of_list xs = match xs with
    | x :: xs' -> Cons (x, phi_of_list xs')
    | []       -> Nil

  let rec phi_list_append xs ys = match xs with
    | Cons (x, xs') -> phi_list_append xs' (Cons (x, ys))
    | Nil           -> ys
    | _             -> raise (PhiMalformedListExn xs)
end


module Useful = struct
  let k x _ = x

  let option_map f = function
    | Some x -> Some (f x)
    | None   -> None

  let option_flatmap f = function
    | Some x -> f x
    | None   -> None

  let is_some = function
    | Some _ -> true
    | None   -> false

  let some f x = Some (f x)
end


(* generic + string parsers *)
module PhiParsers = struct
  open Useful
  open PhiVal
  open PhiValUtils

  exception NotAStringParseState of PhiVal.t

  (* String parsers *)
  let str s = function
    | Cons (String v, Int i) ->
        if i + String.length s <= String.length v
        && s = String.sub v i (String.length s)
          then Some (Cons (String s, Cons (String v, Int (i + String.length s))))
          else None
    | x -> raise (NotAStringParseState x)

  let chartest b cs = function
    | Cons (String v, Int i) ->
        if i < String.length v && b = String.contains cs v.[i]
          then Some (Cons (String (String.sub v i 1),
                           Cons (String v, Int (i + 1))))
          else None
    | x -> raise (NotAStringParseState x)

  let oneof  = chartest true
  let noneof = chartest false

  (* General *)
  let empty i = Some (Cons (Nil, i))
  let none  i = None

  (* High-order parsers *)
  let seq f g i = match f i with
    | None                 -> None
    | Some (Cons (r1, i')) -> (match g i' with
      | None                  -> None
      | Some (Cons (r2, i'')) -> Some (Cons (Cons (r1, r2), i''))
      | Some x                -> raise (UhOh x))
    | Some x               -> raise (UhOh x)
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
        | Some (Cons (r, i')) -> rep' (min-1) i' (r :: rs)
        | Some x              -> raise (UhOh x)
        | None                ->
            if min <= 0 then Some (Cons (phi_of_list (List.rev rs), i))
                        else None in
    rep' min i []

  let star = rep 0
  let plus = rep 1

  let p_mbind p f i = match p i with
    | Some (Cons (v, i')) -> f v i'
    | Some x              -> raise (UhOh x)
    | None                -> None

  let p_map p f i = match p i with
    | Some (Cons (v, i')) -> Some (Cons (f v, i'))
    | Some x              -> raise (UhOh x)
    | None                -> None
end


(* phi-specific syntactic definitions *)
module PhiSyntax = struct
  open Useful
  open PhiVal
  open PhiValUtils
  open PhiParsers

  let rec phi_join = function
    | Cons (String x, xs) -> x ^ phi_join xs
    | Nil                 -> ""
    | x                   -> raise (PhiMalformedListExn x)

  let phi_line_comment = p_map (str "#" ++ star (noneof "\n")) (k Nil)
  let phi_whitespace   = p_map (plus (oneof " \t\r\n"))        (k Nil)
  let phi_ignore       = either phi_whitespace phi_line_comment

  let spaced x         = p_map (star phi_ignore ++ x ++ star phi_ignore)
                         (function
                          | Cons (Cons (_, x), _) -> x
                          | x                     -> raise (UhOh x))

  let phi_symbol_char  = oneof ("abcdefghijklmnopqrstuvwxyz_?!"
                              ^ "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  let phi_symbol       = p_map (plus phi_symbol_char)
                         (fun cs -> mksym (phi_join cs))

  let phi_string       = p_map (str "\"" ++ star (noneof "\\\"") ++ str "\"")
                         (function
                          | Cons (Cons (_, x), _) -> String (phi_join x)
                          | x                      -> raise (UhOh x))

  let phi_digit        = oneof "0123456789"
  let phi_integer      = p_map (plus phi_digit)
                         (fun xs -> Int (int_of_string (phi_join xs)))

  let phi_parens x     = p_map (spaced (str "(") ++ x ++ spaced (str ")"))
                         (function
                          | Cons (Cons (_, x), _) -> x
                          | x                     -> raise (UhOh x))
end


(* phi root scope definitions *)
module PhiBoot = struct
  open Useful
  open PhiVal
  open PhiValUtils
  open PhiParsers
  open PhiSyntax

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
  and method_      = mkobj "method"
  and forward_     = mkobj "forward"
  and call_        = mkobj "call"
  and scope_       = mkobj "scope"
  and instance_    = mkobj "instance"
  and parser_      = mkobj "parser"

  let typed_ = mkconstraint "type"
  let type_  = mkmethod "type"
  let size_  = mkmethod "size"
  let val_   = mkmethod "val"

  let h_     = mkmethod "h"
  let t_     = mkmethod "t"

  (* phi instance representation *)
  let iof t x = Cons (Cons (instance_, t), x)
  let is_iof t x = match x with
    | Cons (i, t') -> i = instance_ && t' = t
    | _            -> false

  let typeof x = match x with
    | Cons (i, t) -> if i = instance_ then Some t else None
    | _           -> None

  (* phi calling convention *)
  let x_ = mkvar "x"
  let y_ = mkvar "y"
  let p_ = mkvar "p"
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

  (* term rewriting *)
  (* TODO: support eval as a rewriter *)
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
      | x                       -> x in
    r v

  let is_constant = function
    | Method _ -> false
    | Call _   -> false
    | _        -> true

  (* eval *)
  (* TODO: formal design for eval *)
  let rec constraint_matches scope c v =
    is_constant v
    && match c with
       | Constraint (i, s, t, p) -> is_some (destructure scope t
                                               (eval scope (Method (i, s, v))))
       | _                       -> raise (PhiNotAConstraintExn c)

  and call scope = function
    | Cons (Cons (i, t'), t) -> if i  = instance_
                                && t' = scope_
                                  then apply_scope_dynamic scope t
                                  else k None
    | Nil           -> k None
    | Fn     (l, r) -> fun i -> option_map
                                  (fun x -> eval scope (rewrite r x))
                                  (destructure scope l i)
    | Hosted (l, f) -> fun i -> option_flatmap f (destructure scope l i)
    | Forward (_, { contents = Some x })
                    -> call scope x
    | x             -> raise (PhiNotCallableExn x)

  and apply_scope ps = apply_scope_dynamic ps ps

  and apply_scope_dynamic scope ps i =
    let rec apply' ps = match ps with
      | Nil           -> None
      | Cons (f, ps') ->
        (match f with     (* NB: logic here is meh; could be cleaner *)
          | Cons (i', t') -> if i' = instance_
                             && t' = scope_
                               then apply' ps'
                               else (match call scope f i with
                                     | None -> apply' ps'
                                     | x    -> x)
          | x -> (match call scope f i with
                  | None -> apply' ps'
                  | x    -> x))
      | Forward (_, { contents = Some x }) -> apply' x
      | _ -> raise (PhiMalformedListExn ps) in
    apply' ps

  and eval1 s v = option_map (eval s) (apply_scope s v)
  and eval s v = match apply_scope s v with
    | Some v' -> eval s v'
    | None    -> v

  and resolve v = match v with
    | Forward (n, { contents = Some x }) -> resolve x
    | _                                  -> v

  (* NB: this can be converted into a meta scope that governs how pattern
     matching works: use quoted encodings for functions and args maybe.
     ...but that's a lot of work for not very much purpose. *)

  (* TODO: convert this to parsers. we need some indirection and ability to
     expand function calls on demand, rather than eagerly rewriting. *)
  and destructure scope expr i =
    match expr, resolve i with
      | Variable _ as v, i               -> Some (Cons (Cons (v, i), Nil))
      | Int ne,          Int ni          -> if ne = ni then Some Nil else None
      | String se,       String si       -> if se = si then Some Nil else None
      | Symbol (i1, s1), Symbol (i2, s2) -> if i1 = i2 then Some Nil else None
      | Object (a, _),   Object (b, _)   -> if a  = b  then Some Nil else None
      | Call (ve, ae),   Call (vi, ai)   -> twoparse scope ve ae vi ai
      | Cons (xe, ye),   Cons (xi, yi)   -> twoparse scope xe ye xi yi
      | Nil,             Nil             -> Some Nil

      | Method (he, _, ve), Method (hi, _, vi) ->
          if he = hi then destructure scope ve vi else None

      | Constraint (_,_,_,p) as c, i ->
          if constraint_matches scope c i then destructure scope p i
                                          else None

      (* NB: if we call back into a scope here, we'll infinite-loop for rejected
         patterns. We need to simple-fail. *)
      | _ -> None

  and twoparse scope e1 e2 i1 i2 = match destructure scope e1 i1,
                                         destructure scope e2 i2 with
    | Some re, Some ri -> Some (phi_list_append re ri)
    | _                -> None

  (* Reader *)
  let rec read_continuations s v i =
    let v = eval s v in
    match eval1 s (Call (Call (parse_continuation_ v, s), iof parse_state_ i)) with
      | None                -> Cons (v, i)
      | Some (Cons (x, i')) -> read_continuations s x i'
      | Some x              -> raise (PhiMalformedParseResultExn x)

  let read s =
    let read' i = match apply_scope s i with
      | None                -> None
      | Some (Cons (v, i')) -> Some (read_continuations s v i')
      | Some x              -> raise (PhiMalformedParseResultExn x) in
    let s' = match typeof s with
      | Some t -> if t = scope_
                    then s
                    else raise (PhiNotAScopeTypeExn t)
      | None   -> iof scope_ s in

    function
    | String _               as x ->
        read' (Call (iof parse_state_ (Cons (x, Int 0)), s'))
    | Cons (String _, Int _) as c ->
        read' (Call (iof parse_state_ c, s'))
    | Cons (Cons (i, t), Cons (String _, Int _)) as ps ->
        if i = instance_
        && t = parse_state_
          then read' (Call (ps, s'))
          else raise (NotAStringParseState ps)
    | x -> raise (NotAStringParseState x)

  (* Boot scope *)
  let scope_of_list x = iof scope_ (phi_of_list x)

  let bind name v = Fn (mksym name, v)
  let boot_bindings =
    scope_of_list
    [ bind "int" int_;
      bind "string" string_;
      bind "symbol" symbol_;
      bind "variable" variable_;
      bind "object" object_;
      bind "fn" fn_;
      bind "hosted" hosted_;
      bind "nil" Nil;
      bind "cons" cons_;
      bind "forward" forward_;
      bind "constraint" constraint_;
      bind "scope" scope_;

      bind "method" method_;
      bind "call" call_;
      bind "instance" instance_;
      bind "parser" parser_;
      bind "parse_state" parse_state_ ]

  let int_literal    = spaced phi_integer
  let string_literal = spaced phi_string
  let symbol_literal = p_mbind
                         (spaced phi_symbol)
                         (function
                           | Symbol (_, s) as r ->
                               if s = "end" then k None
                                            else fun i -> Some (Cons (r, i))
                           | x -> raise (UhOh x))

  let method_parser  = spaced (p_map (str "." ++ phi_symbol)
                                     (function
                                      | Cons (_, x) -> x
                                      | x           -> raise (UhOh x)))

  let call_parser = read

  let val_call_parser s v = p_map (call_parser s) (fun x -> Call (v, x))
  let val_method_parser v = p_map method_parser (function
    | Symbol (sh, ss) -> Method (sh, ss, v)
    | x               -> raise (UhOh x))

  let int_fn f = with_arg "x" (function
    | Int n -> f n
    | _     -> None)

  let ints_fn f = with_args "x y" (function
    | [Int x; Int y] -> f x y
    | _              -> None)

  let str_fn f = with_arg "x" (function
    | String s -> f s
    | _        -> None)

  let mcall m x y = Call (mkmethod m x, y)

  (* core syntactic parse continuations *)
  let defcontinuationparser x_arg f =
    Hosted (Call (Call (parse_continuation_ x_arg, s_), iof parse_state_ p_),
            with_args "x s p" (function
              | [x; s; p] -> f x s p
              | x         -> raise (UhOh (phi_of_list x))))

  let method_parse_k = defcontinuationparser x_ (fun x s -> val_method_parser x)
  let call_parse_k   = defcontinuationparser x_ (fun x s -> val_call_parser s x)
  let scope_begin_k  = defcontinuationparser
                         (mkmethod "begin" (typed_ scope_ x_))
                         (fun x _ -> p_map (read x ++ spaced (str "end"))
                                           (function
                                             | Cons (x', _) -> eval x x'
                                             | x -> raise (UhOh x)))

  let core_parse_continuations =
    scope_of_list
    [ method_parse_k;
      scope_begin_k;
      call_parse_k ]

  (* type accessors for wrapped things *)
  let core_type_continuations =
    scope_of_list
    [ Fn (type_ (iof x_ p_), x_);
      Fn (mkmethod "detype" (iof p_ x_), x_);
      Fn (mcall    "entype" x_ (typed_ object_ p_), iof p_ x_) ]

  let scope_functions =
    scope_of_list
    [ Hosted (mcall "eval" x_ y_,
              with_args "x y" (function
                | [x; y] -> Some (eval x y)
                | x      -> raise (UhOh (phi_of_list x))));
      Fn (h_ (iof scope_ (Cons (x_, y_))), x_);
      Fn (t_ (iof scope_ (Cons (x_, y_))), iof scope_ y_);
      Fn (mcall "cons" (iof scope_ x_) y_,
          iof scope_ (Cons (y_, x_))) ]

  let list_functions =
    scope_of_list
    [ Fn (h_ (iof cons_ (Cons (x_, y_))), x_);    (* wrapped cells *)
      Fn (t_ (iof cons_ (Cons (x_, y_))), y_);

      Fn (h_ (Cons (x_, y_)), x_);                (* unwrapped cells *)
      Fn (t_ (Cons (x_, y_)), y_);
      Fn (t_ Nil, Nil);
      Fn (mkmethod "count" Nil, Int 0);

      Fn (mkmethod "count" x_, mcall "plus" (Int 1)
                                 (mkmethod "count" (t_ x_)));

      Fn (mkmethod "nil?" Nil, Int 1);
      Fn (mkmethod "nil?" x_,  Int 0);

      Fn (Call (Call (cons_, x_), y_), Cons (x_, y_)) ]

  let int_functions =
    scope_of_list
    [ Hosted (mcall "plus" (typed_ int_ x_) (typed_ int_ y_),
              ints_fn (fun x y -> Some (Int (x + y))));
      Hosted (mcall "times" (typed_ int_ x_) (typed_ int_ y_),
              ints_fn (fun x y -> Some (Int (x * y))));

      Hosted (mkmethod "neg" (typed_ int_ x_),
              int_fn (fun x -> Some (Int (-x))));

      Hosted (mcall "lt" (typed_ int_ x_) (typed_ int_ y_),
              ints_fn (fun x y -> Some (Int (if x < y then 1 else 0))));
      Hosted (mcall "le" (typed_ int_ x_) (typed_ int_ y_),
              ints_fn (fun x y -> Some (Int (if x <= y then 1 else 0))));
      Hosted (mcall "eq" (typed_ int_ x_) (typed_ int_ y_),
              ints_fn (fun x y -> Some (Int (if x = y then 1 else 0))));

      Hosted (mcall "if" (typed_ int_ p_) (Call (x_, y_)),
              with_args "p x y" (function
                | [Int p; x; y] -> Some (if p != 0 then x else y)
                | _             -> raise (UhOh Nil))) ]

  let string_functions =
    scope_of_list
    [ Hosted (size_ (typed_ string_ x_),
              str_fn (fun x -> Some (Int (String.length x))));
      Hosted (mkmethod "sym" (typed_ string_ x_), str_fn (some mksym));
      Hosted (mkmethod "var" (typed_ string_ x_), str_fn (some mkvar));
      Hosted (mkmethod "obj" (typed_ string_ x_), str_fn (some mkobj)) ]

  let forward_functions =
    scope_of_list
    [ Hosted (val_ (typed_ forward_ x_),
              (function
                | Forward (_, { contents = x }) -> x
                | x                             -> raise (UhOh x))) ]

  (* TODO: how do we want to handle symbol -> value bindings? *)
  let symbol_functions =
    scope_of_list
    [ ]

  let scope_ref_parser =
    Hosted (Call (iof parse_state_ x_, iof scope_ y_),
            with_args "x y" (function
              | [x; y] -> p_map (str "scope") (k y) x
              | x      -> raise (UhOh (phi_of_list x))))

  let literal_parser p = Hosted (Call (iof parse_state_ x_, iof scope_ y_),
                                 with_arg "x" p)
  let literal_parsers =
    scope_of_list
    (scope_ref_parser ::
     List.map literal_parser
              [ int_literal;
                string_literal;
                symbol_literal ])

  let paren_parsers =
    scope_of_list
    [ Hosted (Call (iof parse_state_ x_, iof scope_ y_), with_args "x y"
        (function
          | [x; y] -> phi_parens (read y) x
          | _      -> raise (UhOh Nil))) ]

  let rec typeof = function
    | Int _        -> Some int_
    | String _     -> Some string_
    | Symbol _     -> Some symbol_
    | Variable _   -> Some variable_
    | Object _     -> Some object_
    | Fn _         -> Some fn_
    | Hosted _     -> Some hosted_
    | Nil          -> Some Nil
    | Cons _       -> Some cons_
    | Constraint _ -> Some constraint_
    | Forward _    -> Some forward_
    | _            -> None

  (* TODO:
     for operator precedence parsing, we can continue with modified scopes that
     locally remove parse alternatives for lower-precedence operators (I think).

     This can be written inside phi.
  *)
  let boot_scope_ref = mkforward ()
  let boot_scope = scope_of_list (
    [ boot_bindings;
      core_parse_continuations;
      core_type_continuations;
      scope_functions;
      list_functions;
      int_functions;
      string_functions;
      symbol_functions;
      forward_functions;
      literal_parsers;
      paren_parsers ]
    @
    [
      bind "boot_bindings" boot_bindings;
      bind "core_parse_continuations" core_parse_continuations;
      bind "core_type_continuations" core_type_continuations;
      bind "scope_functions" scope_functions;
      bind "list_functions" list_functions;
      bind "int_functions" int_functions;
      bind "string_functions" string_functions;
      bind "symbol_functions" symbol_functions;
      bind "forward_functions" forward_functions;
      bind "phi_root" boot_scope_ref;
      bind "literal_parsers" literal_parsers;
      bind "paren_parsers" paren_parsers;

      bind "method_parser" method_parse_k;
      bind "call_parser" call_parse_k;

      Hosted (type_ x_, with_arg "x" typeof);
    ])

  let () = forward_set boot_scope_ref boot_scope
end

open PhiVal
open PhiValUtils
open PhiParsers
open PhiSyntax
open PhiBoot

let typed_explain s v =
  let t = eval s (type_ v) in
  explain v ^ " : " ^ explain t

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let scope' =
  let p = read boot_scope (String (load_file "core.phi")) in
  match p with
    | Some (Cons (x, _)) -> x
    | _                  -> print_string "failed to parse\n"; boot_scope

let _ = print_string (explain scope' ^ "\n")

let rec repl () =
  try let s  = input_line stdin in
      let p  = read scope' (String s) in
      let p' = match p with
        | Some (Cons (x, _)) -> eval scope' x
        | _                  -> String ("failed to parse " ^ s) in
      let ex = match p with
        | Some (Cons (_, Cons (String s, Int n))) ->
            (String.sub s 0 n) ^ "|" ^ (String.sub s n (String.length s - n))
        | _ -> "" in
      let () = print_string (ex ^ " = " ^ typed_explain boot_scope p' ^ "\n") in
      repl ()
  with End_of_file -> ()

let () = repl ()
