## Next steps
0. Rewrite everything using an indirect-threaded machine code interpreter with
   hosted GC functions (do we get reflective data structures this way?)

1. Fix infix/concatenative arity problem
2. Save the infix language as such by writing a simple concatenative compiler
   for optrees (if it's straightforward)
3. Save everything by finishing out the abstract interpreter and figuring out
   where the JIT boundary is
4. Implement LHS destructuring parsers+syntax for infix
