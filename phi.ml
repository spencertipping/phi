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

module ParserCombinators = struct
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
  let emit x = Some ([x], [])
  let match_method m p x =
    match x with
      | Method (h, v) -> if m = h then p v else None
      | _             -> None

  let match_call pv pa x =
    match x with
      | Call (v, a) -> match pv v, pa a with
        | Some (rv, kv), Some (ra, ka) -> Some (rv @ ra, kv @ ka)
        | None                         -> None
      | _           -> None

  let empty i = Some ((), i)

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

open PhiVal
open ParserCombinators
