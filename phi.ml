(*
    phi programming language
    Copyright (C) 2018  Spencer Tipping

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open Array
open Bytes
open String
open Sys

let now = let open Unix in gettimeofday

(* general interpreter settings *)
let print_on_crash = false
let print_each_insn = try getenv "PHI_PRINT_EACH_INSN" = "yes"
                      with Not_found -> false

type native_val =
  | InChannel  of in_channel
  | OutChannel of out_channel

module rec PhiV : sig
  type t =
    | Nil
    | Cons   of PhiV.t * PhiV.t
    | Int    of int
    | Real   of float
    | Str    of bytes
    | Sym    of int * string
    | Mut    of PhiV.t option ref
    | Native of native_val
end = struct
  type t =
    | Nil
    | Cons   of PhiV.t * PhiV.t
    | Int    of int
    | Real   of float
    | Str    of bytes
    | Sym    of int * string
    | Mut    of PhiV.t option ref
    | Native of native_val
end

open PhiV

let rec string_of_phiv = function
  | Nil         -> "nil"
  | Cons (x, y) -> "(" ^ string_of_phiv x ^ " :: " ^ string_of_phiv y ^ ")"
  | Int i       -> string_of_int i
  | Real r      -> string_of_float r
  | Str s       -> "\"" ^ Bytes.to_string s ^ "\""
  | Sym (_, s)  -> "'" ^ s
  | Mut _       -> "M[...]"
  | Native _    -> "NATIVE"

let print_phiv c v = output_string c (string_of_phiv v)

exception ThisShouldNeverHappen
exception DerefNullMutExn
exception StackUnderflowExn
exception UnimplementedExn

exception BogusArgsExn of int * PhiV.t
exception ExpectedAnInt of PhiV.t
exception ExpectedAList of PhiV.t
exception IllegalInsnExn of PhiV.t
exception CrashExn of PhiV.t

type phii = PhiV.t * PhiV.t * PhiV.t

let sym_of_str s =
  let h = Hashtbl.hash s in
  Sym (h, s)

let t_nil    = sym_of_str "nil"
let t_cons   = sym_of_str "cons"
let t_int    = sym_of_str "int"
let t_real   = sym_of_str "real"
let t_str    = sym_of_str "str"
let t_sym    = sym_of_str "sym"
let t_mut    = sym_of_str "mut"
let t_native = sym_of_str "native"

exception DerefAMutBomb

let rec deref = function
  | Mut { contents = Some x } as m -> if x == m
                                        then raise DerefAMutBomb
                                        else deref x
  | Mut { contents = None }        -> raise DerefNullMutExn
  | x                              -> x

let rec typeof = function
  | Mut { contents = Some x } -> typeof x
  | Mut { contents = None }   -> t_mut
  | Nil                       -> t_nil
  | Cons _                    -> t_cons
  | Int _                     -> t_int
  | Real _                    -> t_real
  | Str _                     -> t_str
  | Sym _                     -> t_sym
  | Native _                  -> t_native

exception NthCellExpectedACons of PhiV.t
let rec nthcell x = function
  | 0 -> x
  | n -> match deref x with
    | Cons (_, x') -> nthcell x' (n-1)
    | x'           -> raise (NthCellExpectedACons x')

let rec phimap_onto e f xs = match deref xs with
  | Cons (v, xs') -> Cons (f v, phimap_onto e f xs')
  | Nil           -> e
  | x             -> raise (ExpectedAList x)

exception NthRefShouldBeACons of PhiV.t
exception RestackArgShouldBeACons of PhiV.t
let restack d = function
  | Cons (n, is) -> (match deref n with
    | Int n' ->
      phimap_onto
        (nthcell d n')
        (fun x -> match deref x with
           | Int i -> (match deref (nthcell d i) with
             | Cons (h, _) -> h
             | x           -> raise (NthRefShouldBeACons x))
           | x     -> raise (ExpectedAnInt x))
        is
    | x -> raise (ExpectedAnInt x))
  | x -> raise (RestackArgShouldBeACons x)

let unop f d = match deref d with
  | Cons (x, d') -> f (deref x, d')
  | _ -> raise StackUnderflowExn

let binop f d = match deref d with
  | Cons (x, d') -> (match deref d' with
    | Cons (y, d'') -> f (deref x, deref y, d'')
    | _ -> raise StackUnderflowExn)
  | _ -> raise StackUnderflowExn

let ternop f d = match deref d with
  | Cons (x, d') -> (match deref d' with
    | Cons (y, d'') -> (match deref d'' with
      | Cons (z, d''') -> f (deref x, deref y, deref z, d''')
      | _ -> raise StackUnderflowExn)
    | _ -> raise StackUnderflowExn)
  | _ -> raise StackUnderflowExn

let quadop f d = match deref d with
  | Cons (x, d') -> (match deref d' with
    | Cons (y, d'') -> (match deref d'' with
      | Cons (z, d''') -> (match deref d''' with
        | Cons (a, d'''') -> f (deref x, deref y, deref z, deref a, d'''')
        | _ -> raise StackUnderflowExn)
      | _ -> raise StackUnderflowExn)
    | _ -> raise StackUnderflowExn)
  | _ -> raise StackUnderflowExn

let fiveop f d = match deref d with
  | Cons (x, d') -> (match deref d' with
    | Cons (y, d'') -> (match deref d'' with
      | Cons (z, d''') -> (match deref d''' with
        | Cons (a, d'''') -> (match deref d'''' with
          | Cons (b, d''''') -> f (deref x, deref y, deref z,
                                   deref a, deref b, d''''')
          | _ -> raise StackUnderflowExn)
        | _ -> raise StackUnderflowExn)
      | _ -> raise StackUnderflowExn)
    | _ -> raise StackUnderflowExn)
  | _ -> raise StackUnderflowExn

let unop_int f op d c r = unop (function
  | (Int x, d') -> (Cons(Int (f x), d'), c, r)
  | _           -> raise (BogusArgsExn (op, d))) d

let binop_int f op d c r = binop (function
  | (Int x, Int y, d') -> (Cons(Int (f x y), d'), c, r)
  | _                  -> raise (BogusArgsExn (op, d))) d

exception UnconsArgExpectedCons of PhiV.t
exception RestackExpectedCons   of PhiV.t
exception NotAMut               of PhiV.t

let last_time_print = ref 0.
let t0 = now ()

let eval (d, c, r) insn =
  if print_each_insn
    then let t = now () in
         if t -. !last_time_print > 0.01
           then (print_string "TIME\t";
                 print_float (t -. t0);
                 print_string "\n";
                 last_time_print := t)
           else ()
    else ();
  match deref insn with
    | Mut _         -> raise ThisShouldNeverHappen
    | Nil           -> (Cons(Nil, d), c, r)
    | Cons _   as x -> (Cons(x, d),   c, r)
    | Str _    as x -> (Cons(x, d),   c, r)
    | Real _   as x -> (Cons(x, d),   c, r)
    | Native _ as x -> (Cons(x, d),   c, r)

    | Sym _  as x -> (Cons(x, d), Cons(r, Cons(Int 2, c)), r)
    | Int i -> match i with
      | 0x00 -> (Cons(Cons(d, Cons(c, Cons(r, Nil))), d), c, r)
      | 0x01 -> (match deref d with
                   | Cons(c', d') -> (d', c', r)
                   | _            -> raise StackUnderflowExn)
      | 0x02 -> (match deref d with
                   | Cons(x, d') -> (d', Cons(x, c), r)
                   | _           -> raise StackUnderflowExn)
      | 0x03 -> (match deref d with
                   | Cons(x, d') -> (Cons(typeof x, d'), c, r)
                   | _           -> raise StackUnderflowExn)
      | 0x04 -> raise UnimplementedExn
      | 0x05 -> (match deref d with
                   | Cons(h, d') -> (match deref d' with
                     | Cons(t, d'') -> (Cons(Cons(h, t), d''), c, r)
                     | _            -> raise StackUnderflowExn)
                   | _           -> raise StackUnderflowExn)
      | 0x06 -> (match deref d with
                   | Cons(x, d') -> (match deref x with
                     | Cons(h, t) -> (Cons(h, Cons(t, d')), c, r)
                     | x          -> raise (UnconsArgExpectedCons x))
                   | _           -> raise StackUnderflowExn)
      | 0x07 -> (match deref d with
                   | Cons(x, d') -> (restack (deref d') (deref x), c, r)
                   | x           -> raise (RestackExpectedCons x))
      | 0x08 -> (Cons(Mut (ref None), d), c, r)
      | 0x09 -> (match deref d with
                   | Cons(v, d') -> (match deref d' with
                     | Cons(Mut m as x, d'') -> m := Some v; (x, c, r)
                     | x                     -> raise (NotAMut x))
                   | _           -> raise StackUnderflowExn)
      | 0x0a -> (match deref d with
                   | Cons(d', _) -> (d', c, r)
                   | _           -> raise StackUnderflowExn)
      | 0x0b -> (match deref d with
                   | Cons(r', _) -> (d, c, r')
                   | _           -> raise StackUnderflowExn)

      | 0x0c -> (match deref d with
                   | Cons(el, d') -> (match deref d' with
                     | Cons(th, d'') -> (match deref d'' with
                       | Cons(Int i, d''') ->
                         (d''', Cons((if i != 0 then th else el), c), r)
                       | x                 -> raise (ExpectedAnInt x))
                     | _             -> raise StackUnderflowExn)
                   | _            -> raise StackUnderflowExn)

      (* integer ops *)
      | 0x10 -> binop_int ( + )    0x10 d c r
      | 0x11 -> unop_int  ( ~- )   0x11 d c r
      | 0x12 -> binop_int ( * )    0x12 d c r
      | 0x13 -> raise UnimplementedExn      (* divmod *)
      | 0x14 -> binop_int ( lsl )  0x14 d c r
      | 0x15 -> binop_int ( asr )  0x15 d c r
      | 0x16 -> binop_int ( land ) 0x16 d c r
      | 0x17 -> binop_int ( lxor ) 0x17 d c r
      | 0x18 -> unop_int  ( lnot ) 0x18 d c r
      | 0x19 -> binop_int ( fun x y -> if x <  y then 1 else 0 ) 0x19 d c r
      | 0x1a -> unop_int  ( fun x   -> if x == 0 then 1 else 0 ) 0x1a d c r

      (* string/symbol ops *)
      | 0x20 -> unop (function
        | (Int size, d') -> (Cons(Str(Bytes.make size '\000'), d'), c, r)
        | _              -> raise (BogusArgsExn (0x20, d))) d
      | 0x21 -> unop (function
        | (Str s, d') -> (Cons(Int(Bytes.length s), d'), c, r)
        | _           -> raise (BogusArgsExn (0x21, d))) d
      | 0x22 -> binop (function
        | (Int i, Str s, d') -> (Cons(Int(int_of_char (Bytes.get s i)), d'), c, r)
        | _                  -> raise (BogusArgsExn (0x22, d))) d
      | 0x23 -> ternop (function
        | (Int ch, Int i, (Str s as s'), d') ->
            Bytes.set s i (char_of_int ch); (Cons(s', d'), c, r)
        | _ -> raise (BogusArgsExn (0x23, d))) d
      | 0x24 -> binop (function
        | (Str a, Str b, d') -> (Cons(Int(Bytes.compare a b), d'), c, r)
        | _ -> raise (BogusArgsExn (0x24, d))) d
      | 0x25 -> unop (function
        | (Str s, d') -> (Cons(sym_of_str(Bytes.to_string s), d'), c, r)
        | _ -> raise (BogusArgsExn (0x25, d))) d
      | 0x26 -> unop (function
        | (Sym (_, s), d') -> (Cons(Str(Bytes.of_string s), d'), c, r)
        | _ -> raise (BogusArgsExn (0x26, d))) d
      | 0x27 -> binop (function
        | (Sym(i1, s1), Sym(i2, s2), d') ->
            let same  = i1 == i2 && String.equal s1 s2 in
            let samei = if same then 1 else 0 in
            (Cons(Int samei, d'), c, r)
        | _ -> raise (BogusArgsExn (0x27, d))) d
      | 0x28 -> fiveop (function
        | (Int len, Int too, (Str tos as s'),
                    Int fro, Str frs, d') ->
            Bytes.blit frs fro tos too len; (Cons(s', d'), c, r)
        | _ -> raise (BogusArgsExn (0x28, d))) d

      (* real ops TBD *)

      (* interpreter ops *)
      | 0x40 -> (Cons(Int 0, d), c, r)
      | 0x41 -> raise (CrashExn d)
      | 0x42 -> unop (function
        | (Sym(_, "posix_fileio"), d') -> (Cons (Int 1, d'), c, r)
        | (_, d')                      -> (Cons (Int 0, d'), c, r)) d

      (* dev hackery (not a real extension) *)
      | 0x100 -> unop (function
        | (Str s, d') -> if print_each_insn
                           then ()
                           else (print_string s; flush stdout);
                         (d', c, r)
        | _           -> raise (BogusArgsExn (0x100, d))) d

      | 0x101 -> unop (function (x, d') ->
                         if print_each_insn
                           then ()
                           else (print_phiv stdout x; flush stdout);
                         (d', c, r)) d

      | 0x102 -> (Cons((try Str(Bytes.of_string (read_line ()))
                        with End_of_file -> Nil), d), c, r)

      | 0x103 -> unop (function
        | (v, d') -> (Cons(Int(Hashtbl.hash v), d'), c, r)) d

      (* posix_fileio *)
      | 0x110 -> ternop (function
        | (Int mode, Int flags, Str name, d') ->
            let chan = open_in_bin name in
            (Cons(Native(InChannel chan), d'), c, r)
        | _ -> raise (BogusArgsExn (0x110, d))) d

      | 0x111 -> quadop (function
        | (Int len, Int offset, Str s, Native(InChannel i), d') ->
            let rec read_all sofar =
              try
                if sofar < len
                  then match input i s (offset+sofar) (len-sofar) with
                    | 0 -> (Cons(Int(sofar), d'), c, r)
                    | n -> read_all (sofar+n)
                  else (Cons(Int(sofar), d'), c, r)
              with
                End_of_file -> (Cons(Int(sofar), d'), c, r) in
            read_all 0

        | _ -> raise (BogusArgsExn (0x111, d))) d

      | _ -> raise (IllegalInsnExn insn)

let rec next_insn c = match deref c with
  | Cons(ch, ct) -> (match deref ch with
    | Nil            -> next_insn ct
    | Cons(chh, cht) -> Some (chh, Cons(cht, ct))
    | _              -> Some (ch, ct))
  | Nil -> None
  | i -> raise (IllegalInsnExn i)

let step (d, c, r) = match next_insn c with
  | Some (i, c') -> (if print_each_insn
                       then (match i with
                             | Int i' -> print_string "INSN\t";
                                         print_int i';
                                         print_string "\n"
                             | Cons _ -> print_string "INSN\tC";
                                         print_int (Hashtbl.hash i);
                                         print_string "\n"
                             | Nil    -> print_string "INSN\tN\n"
                             | _      -> ())
                       else ();
                     try Some (eval (d, c', r) i)
                     with e ->
                       if print_on_crash
                         then (output_string stderr "CRASHED\n";
                               output_string stderr "d = ";
                               print_phiv stderr d;
                               output_string stderr "\nc = ";
                               print_phiv stderr c;
                               output_string stderr "\nr = ";
                               print_phiv stderr r;
                               output_string stderr "\n\n";
                               raise e)
                         else raise e)
  | None         -> None

let rec run i = match step i with
  | Some i' -> run i'
  | None    -> i

(* binary image loading *)
exception IllegalImageByteExn of int
exception SymOfNonStr         of PhiV.t

let input_le_int c =
  let b1 = input_byte c in
  let b2 = input_byte c in
  let b3 = input_byte c in
  let b4 = input_byte c in
  (b4 lsl 24) lor (b3 lsl 16) lor (b2 lsl 8) lor b1

let input_le_int64 c =
  let b1 = input_byte c in
  let b2 = input_byte c in
  let b3 = input_byte c in
  let b4 = input_byte c in
  let b5 = input_byte c in
  let b6 = input_byte c in
  let b7 = input_byte c in
  let b8 = input_byte c in
  (b8 lsl 56) lor (b7 lsl 48) lor (b6 lsl 40) lor (b5 lsl 32) lor
  (b4 lsl 24) lor (b3 lsl 16) lor (b2 lsl 8) lor b1

let input_bytes c size =
  let b = Bytes.create size in
  really_input c b 0 size; b

let read_a_thing values = try
  match input_byte stdin with
    | 0x00 -> Some (1, Nil)
    | 0x01 -> let h = Array.get values (input_le_int stdin) in
              let t = Array.get values (input_le_int stdin) in
              Some (9, Cons (h, t))
    | 0x02 -> Some (9, Int (input_le_int64 stdin))
    | 0x03 -> let size = input_le_int stdin in
              Some (size + 5, Str (input_bytes stdin size))
    | 0x04 -> (match Array.get values (input_le_int stdin) with
                 | Str s -> Some (5, sym_of_str (Bytes.to_string s))
                 | x     -> raise (SymOfNonStr x))
    | 0x05 -> Some (5, Mut (ref (Some (Int (input_le_int stdin)))))
    | 0x06 -> raise UnimplementedExn
    | x    -> raise (IllegalImageByteExn x)
  with End_of_file -> None

exception UnknownCLIArgsExn of string array
let _ =
  let isize  = input_le_int stdin in
  let values = Array.make (isize / 5) Nil in
  let rec read_next i offset mutlist =
    if offset < isize
      then match read_a_thing values with
        | Some (size, (Mut _ as v)) -> values.(i) <- v;
                                       read_next (i+1) (offset+size) (v::mutlist)
        | Some (size, v)            -> values.(i) <- v;
                                       read_next (i+1) (offset+size) mutlist
        | None                      -> (values.(i-1), i, mutlist)
      else (values.(i-1), i, mutlist) in
  let (vfinal, n, mutlist) = read_next 0 0 [] in
  let rec set_muts_in = function
    | m::ms' -> (match m with
      | Mut ({ contents = Some (Int i) } as r) -> r := Some (Array.get values i);
                                                  set_muts_in ms'
      | _ -> raise ThisShouldNeverHappen)
    | [] -> () in
  set_muts_in mutlist;
  flush stderr;
  match argv with
    | [| _ |]                           (* no args: run normally *)
      -> let _ = run (Nil, Cons(vfinal, Nil), Nil) in ()

    | [| _; "--lookup"; h |]            (* lookup phi value by ocaml hash *)
      -> let hi = int_of_string h in
         let rec find_value i =
           if i < n
             then if Hashtbl.hash values.(i) = hi
               then values.(i)
               else find_value (i + 1)
             else raise Not_found in
         print_phiv stdout (find_value 0);
         print_string "\n"

    | _ -> raise (UnknownCLIArgsExn argv)
