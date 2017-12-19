# Ref codes
Every `ref` has an integer code that uniquely identifies it. You don't normally
access ref codes, but you can see them using `quote`. This table lists the refs
that are builtin and their corresponding integer codes.

Ref codes between 0 and 255 are reserved for phi bootstrap purposes and will
never be assigned to new refs. If you unquote a new ref into this range, your
program may have unspecified behavior.

## Optimization and redundancy
phi keeps its backend dependencies minimized; for example, `iminus_op` can be
defined in terms of `iplus_op` and `ineg_op`, so you don't have to implement
`iminus_op`. However, there may be cases where a backend provides optimized
behavior and you want to use it when possible. This is done using structural
parsers that look for specific patterns and emit optimized operations.

## Ref code assignments
Code | Name | Description
-----|------|------------
0   | `nil`        | the end of a list
1   | `instance`   | catalyzes type application
2   | `op`         | catalyzes op application
    |              |
4   | `cons_t`     | the type of a cons cell
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
96  | `p_vvar_t`   | a parser that binds a variable
97  | `p_vtype_t`  | a parser that asserts a type
    |              |
128 | `quote_op`   | catalyzes introspection
129 | `unquote_op` | removes `quote_op`
130 | `typeof_op`  | returns the computed type of a value
131 | `parse_op`   | applies a parser to a parse state
132 | `rewrite_op` | rewrites variables with a simple scope
    |              |
144 | `iplus_op`   | adds two integers
145 | `ineg_op`    | negates an integer
146 | `itimes_op`  | multiplies two integers
147 | `iinv_op`    | bit inversion of two integers
148 | `iand_op`    | logical bit-and of two integers
149 | `inot_op`    | nonzero -> `0`, zero -> `1`
150 | `ixor_op`    | logical bit-xor of two integers
151 | `ilsr_op`    | zero-fill shift right
152 | `ilsl_op`    | zero-fill shift left
153 | `ilt_op`     | returns `1` if arg1 < arg2, `0` otherwise
154 | `iif_op`     | `(if a b c)` = `a ? b : c`
    |              |
160 | `vnew_op`    | create a new val of the specified length
161 | `vlength_op` | length of a val object, in bytes
162 | `vget_op`    | indexed val byte getter
163 | `vset_op`    | modifies one byte of a val in place
    |              |
176 | `cons_op`    | conses two values
177 | `head_op`    | the head of a cons cell
178 | `tail_op`    | the tail of a cons cell
179 | `hset_op`    | changes the head of a cons cell
180 | `tset_op`    | changes the tail of a cons cell
