# Design notes in no particular order
We could have a C program that did OOP with a vtable pointer at the start of
each object. That vtable could easily contain primitive operations for tracing
GC, reporting the object size, moving itself within memory, etc. That vtable
could also itself be a parser.

**Q:** how does the parser differentiate between source continuations
(compile-mode?) and runtime continuations?
