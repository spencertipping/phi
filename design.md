# Design notes in no particular order
We could have a C program that did OOP with a vtable pointer at the start of
each object. That vtable could easily contain primitive operations for tracing
GC, reporting the object size, moving itself within memory, etc. That vtable
could also itself be a parser.

We need this to be concatenative. An integer is "evaluated" by pushing itself
onto the stack, which is, conveniently, a fixed point.

phi is basically a FORTH based on parsers rather than one-layer concatenative
design. This demands some complexity from the interpreter because we need to be
able to use parsers as incremental reducers over inputs, much like the FORTH
interpreter consumes input as it goes. I think this means the return stack is
actually comprised of parser-continuations that conditionally add things.

...and this means the REPL is really a value whose parse continuation
side-effectfully reads input and generates values.

**Q:** how do we manage memory for parse states, given that failover is
theoretically always possible? Maybe buffering is done by `alt` elements, which
tail-call into their final option.

**Q:** should the stack-top value assume control over the stack parse?

**Q:** if we're living in a world of alternatives/etc, how do we encode nested
parsers? Do we have nonlocal returns? (Arguably no, since `alt` has bounded
failure.)
