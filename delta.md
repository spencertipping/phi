## Current problems
1. Functions don't support any destructuring bind syntax
2. The infix interpreter is unusably slow

(2) prevents us from writing further improvements in the infix language, so it
makes some sense to fix that first. We should also fix (1) concatenatively
because the infix language should self-host to a fixed point.

## Next steps
1. Save the infix language as such by writing a simple concatenative compiler
   for optrees (if it's straightforward)
2. Save everything by finishing out the abstract interpreter and figuring out
   where the JIT boundary is
3. Implement LHS destructuring parsers+syntax for infix

**Q:** is it worth creating a hosted parser+authoring environment for
concatenative code?
