1. Think through the way scopes should be structured (path stuff?)
2. Operator precedence parsing
3. Function notation of some type
4. Let-bindings
5. Type masquerading: how do abstract arg-values work? We can't invoke
   primitives on them if they're masqueraded types; maybe primitives all require
   the objects to use the builtin `cons(instance, type)` representation.
