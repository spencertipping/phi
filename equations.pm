=head1 phi language equations
=head2 Fixed-point (boot) layer
This layer describes phi's builtin semantics in terms of those same semantics.
Values are represented in a quoted (descriptive) form. If you compiled this
code, you would get a phi interpreter. If you ran a meta-interpreter, this code
would be present in a meta-scope that governed the language semantics (and which
you could modify).

=head3 Hosted data types (what we have to work with)
1. Integers
2. Symbols
3. Strings
4. Unknowns
5. Nil, written C<()>
6. Cons cells

We also have some builtin constructors for hosted parsers, but they operate in
terms of the above data types. Parsers are primitive objects whose behavior
isn't fully presented to you (i.e. you can't access all of the methods phi's
runtime uses when it interacts with the parsers you construct).

Scopes are like parsers in that they're hosted objects with some opaque
behaviors. They can also be constructed using hosted functions present in the
root scope.

=head3 Quoted data types (what we want to describe)
The goal here is to define the evaluation function for phi, so we need a few
things:

1. An encoding for constant/pending values
2. An encoding for parsers and scopes
3. Enough machinery to be able to write rewriting equations

It's fine for us to defer to backends for a lot of the optimization details. We
just need to give them enough to work with that they _can_ optimize when
appropriate.
