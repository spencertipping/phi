=head1 Bytecode idea
I think it's possible to do all of this stuff using a Forth-like concatenative
setup. Then we get a fast bytecode without too much work, and a JIT if we want
one.

It's worth condensing the low-level semantics a bit. Stuff about maps is silly
and overly complex; really we want to think about things in terms of these
parser bytecodes that form the execution layer.
