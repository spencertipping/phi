# Next steps
1. Pause `phiabstract`'s indirected interpreter; I don't think we need it yet
2. Clip the unit tests accordingly (and wrt shelved modules)
3. Add basic parse continuation logic to `phiabstract` elements
4. Implement custom op elements, one per syntax op? (sure, specialization is
   free)

- Divergence point: do we keep the backends tied to concatenative?
  - If yes:
    5. Back (4) into a concatenative compiler
    6. Write an abstract concatenative interpreter
  - If no:
    5. Back (4) into a non-concatenative interpreter
    6. Abstract interpreter -> compiler?
