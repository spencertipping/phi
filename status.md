# Quick status update: 2018.0905
phi2 is working but a little slow; right now `phi1.elf` clocks in at 4.7 seconds
of runtime and allocates 2.5MB of heap space. There's a noticeable pause for the
phi3 sources, which are at this point fairly minimal.

All of this is fine, but we have some important decisions to make and things we
need to do:

1. Profiling for phi0/phi1?
2. Trivial bytecode -> x86 JIT?
3. Bytecode -> bytecode optimization?
4. Method call optimization?

The phi2 language design also matters, and there are a few things I want to fix:

1. Real CTTI typing instead of this string-mashing silliness
2. Replace ANF gensyms with numeric offsets and better code
3. Finalize the design for managed CTTIs

I'm going to start with the performance stuff because it's a good opportunity to
learn about things that will later be implemented for real in phi2.
