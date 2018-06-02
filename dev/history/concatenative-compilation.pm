=head1 Compiling concatenative code
...should be straightforward; our instruction set doesn't support any open-ended
stack accessors; that is, no runtime data informs the index of any given stack
operation. This means we should be able to erase the stack entirely and get a
register-style representation of any given function.


=head2 Targeting machine code
Our bytecode interpreter is probably fast enough that we can just interpret
stuff; but when we need more performance we should be able to compile hot
methods to save cycles. I think the idea is to go two steps:

1. Concatenative -> SSA or RTL
2. SSA/RTL -> register-allocated machine code native

Easy enough, and low-priority given how fast our bytecode interpreter is.


=head2 Targeting perl
I suspect a lot of this also applies to python/ruby, but let's start with just
perl.

SSA or some register format is a good starting point here. No sense in emulating
the stack; array accesses are a _lot_ more expensive than processor
C<push>/C<pop> instructions.

Q: how do struct accessors work? Do we generate custom natives? (Because if
that's the plan, we'll need to be able to inline those natives if we want any
kind of performance.)
