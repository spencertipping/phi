# Ref codes
Every `ref` has an integer code that uniquely identifies it. You don't normally
access ref codes, but you can see them using `quote`. This table lists the refs
that are builtin and their corresponding integer codes.

Ref codes between 0 and 65535 are reserved for phi bootstrap purposes and will
never be assigned to new refs. If you unquote a new ref into this range, your
program may have unspecified behavior.

Code | Name | Description
-----|------|------------
0  | `instance` | catalyzes type application
1  | `op`       | catalyzes op application
   |            |
2  | `cons_t`   | the type of cons cells
3  | `val_t`    | the type of val objects
4  | `ref_t`    | the type of ref objects
5  | `int_t`    | the type of ints, encoded with val objects
6  | `string_t` | the type of strings, encoded with val objects
7  | `symbol_t` | the type of symbols, encoded with val objects
8  | `var_t`    | the type of variables, encoded with val objects
9  | `scope_t`  | the type of scopes, encoded as cons lists
10 | `ps_str_t` | the type of parse states over strings
11 | `ps_val_t` | the type of parse states over structural values
   |            |
12 | `quote_op`   | catalyzes introspection
13 | `unquote_op` | removes `quote_op`
