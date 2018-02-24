# Next steps
1. Pause `phiabstract`'s indirected interpreter; I don't think we need it yet
2. Clip the unit tests accordingly (and wrt shelved modules)
3. Add basic parse continuation logic to `phiabstract` elements
4. Implement custom op elements, one per syntax op? (sure, specialization is
   free)
5. Back (4) into a concatenative compiler
6. Write an abstract concatenative interpreter
7. Back (6) into applicative languages using continuation parsers

I think we want to implement abstract optimization concatenatively, then use
continuation parsers. Anything else seems like more work.

**Q:** are continuation parsers a reasonable way to optimize? Maybe jointly
parse continuation and data.
