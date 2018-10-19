=head1 About to rewrite almost everything
Here's what's up.

I've got a working phi0/phi1 boot layer. We have unit tests, system calls, etc,
all functional. The main problem is that none of it is GC-safe; we need to
reimplement it all one way or another.

The main question now is whether we build phi1 up to be the compiler we need, or
whether we use something like Perl to write it. The compiler will be throwaway
either way because phi1 isn't GC-safe.

From here, the fastest strategy is to use Perl and a Lisp-style grammar to
generate GC-safe phi0 bytecode. I doubt I'd have understood how to do this
before, but now I'm fairly familiar with how this stuff all works and when we're
GC-safe.
