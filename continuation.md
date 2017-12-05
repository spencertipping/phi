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
