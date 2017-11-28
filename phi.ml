module rec PhiVal : sig
  type t =
  | Nil
  | Int     of int
  | String  of string
  | Cons    of t * t
  | Symbol  of int * string
  | Forward of t ref
  | Method  of int * t
  | Call    of t * t
end = struct
  type t =
  | Nil
  | Int     of int
  | String  of string
  | Cons    of t * t
  | Symbol  of int * string
  | Forward of t ref
  | Method  of int * t
  | Call    of t * t
end

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

  (* phi downward-value parsers *)
  (* Continuation merge points; these don't serialize continuations. *)
  let match_method m p x =
    match x with
      | Method (h, v) -> if m = h then p v else None
      | _             -> None

  let match_call pv pa x =
    match x with
      | Call (v, a) -> (match pv v, pa a with
        | Some (rv, kv), Some (ra, ka) -> Some (rv @ ra, kv @ ka)
        | _                            -> None)
      | _           -> None

  let (^.) p mname = match_method (Hashtbl.hash mname) p
  let (^>)         = match_call

  (* Terminal matchers *)
  let emit x = Some ([x], [])
  let match_int i x = match x with
    | Int n -> if i = n then Some ([i], []) else None
    | _     -> None

  let match_string s x = match x with
    | String s' -> if s = s' then Some ([s], []) else None
    | _         -> None

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
end

module PhiBoot = struct
  open PhiVal
  open PhiParsers
  open PhiSyntax

  let read lscope source = any lscope source
  let eval dscope v      = any dscope v
end

open PhiVal
open PhiParsers
open PhiSyntax
open PhiBoot
