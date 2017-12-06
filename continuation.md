## Strategy
1. Design the evaluator on paper: the role of parsers/etc, and how subexpression
   eval works.
2. Design a core API that gives us enough to work with, including primitive
   access.
3. Rewrite the existing ocaml implementation to follow that spec.

### Core API design
Do we want some type of namespace structuring? If this is going to be familiar
to most programmers, we'll want method-style syntax. Of course, we can always
add this using parse continuations.

## Collected TODO items
1. Think through the way scopes should be structured (path stuff?)
2. Operator precedence parsing
3. Function notation of some type (Q: how to differentiate vars vs symbols?)
    - Inlined function calls and static evaluation?
    - Different var vs function vs method definition forms
4. Let-bindings
5. Type masquerading: how do abstract arg-values work? We can't invoke
   primitives on them if they're masqueraded types; maybe primitives all require
   the objects to use the builtin `cons(instance, type)` representation.
    - NB: nothing wrong with this requirement.
