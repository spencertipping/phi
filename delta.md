# Next steps
1. Add basic parse continuation logic to `phiabstract` elements
2. Implement custom op elements, one per syntax op?
3. Back (2) into a concatenative compiler
4. Write an abstract concatenative interpreter
5. Back (4) into applicative languages using continuation parsers

I think we want to implement abstract optimization concatenatively, then use
continuation parsers. Anything else seems like more work.

**Q:** are continuation parsers a reasonable way to optimize? Maybe jointly
parse continuation and data.
