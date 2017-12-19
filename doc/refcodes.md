# Ref codes
Every `ref` has an integer code that uniquely identifies it. You don't normally
access ref codes, but you can see them using `quote`. This table lists the refs
that are builtin and their corresponding integer codes.

Ref codes between 0 and 65535 are reserved for phi bootstrap purposes and will
never be assigned to new refs. If you unquote a new ref into this range, your
program may have unspecified behavior.

Code | Name | Description
-----|------|------------
0   | `instance`   | catalyzes type application
1   | `op`         | catalyzes op application
    |              |
4   | `cons_t`     | the type of cons cells
5   | `val_t`      | the type of val objects
6   | `ref_t`      | the type of ref objects
7   | `int_t`      | the type of ints, encoded with val objects
8   | `string_t`   | the type of strings, encoded with val objects
9   | `symbol_t`   | the type of symbols, encoded with val objects
10  | `var_t`      | the type of variables, encoded with val objects
11  | `scope_t`    | the type of scopes, encoded as cons lists
    |              |
64  | `ps_str_t`   | the type of parse states over strings
65  | `ps_val_t`   | the type of parse states over structural values
66  | `p_alt_t`    | parser alternation
67  | `p_seq_t`    | parser sequence
68  | `p_rep_t`    | parser repetition
69  | `p_fmap_t`   | parser flatmap
    |              |
80  | `p_sconst_t` | a parser that matches a constant string
81  | `p_sclass_t` | a parser that matches any of a set of chars
    |              |
256 | `quote_op`   | catalyzes introspection
257 | `unquote_op` | removes `quote_op`
258 | `typeof_op`  | returns the computed type of a value
259 | `parse_op`   | applies a parser to a parse state
260 | `rewrite_op` | rewrites variables with a simple scope
    |              |
272 | `iplus_op`   | adds two integers
273 | `ineg_op`    | negates an integer
274 | `itimes_op`  | multiplies two integers
275 | `iinv_op`    | bit inversion of two integers
276 | `iand_op`    | logical bit-and of two integers
277 | `inot_op`    | nonzero -> `0`, zero -> `1`
278 | `ixor_op`    | logical bit-xor of two integers
279 | `ilsr_op`    | zero-fill shift right
280 | `ilsl_op`    | zero-fill shift left
281 | `ilt_op`     | returns `1` if arg1 < arg2, `0` otherwise
282 | `iif_op`     | `(if a b c)` = `a ? b : c`
    |              |
288 | `vlength_op` | length of a val object, in bytes
289 | `vget_op`    | indexed val byte getter
290 | `vset_op`    | returns a new val with a changed byte
