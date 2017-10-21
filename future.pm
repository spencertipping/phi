=head1 State management
Parsers should be immutable objects that we can destructure against and
reconstruct. Then state modifications amount to building a derivative parser,
using it, and throwing it away.


=head1 Operators and inheritance
Suppose arrays support some syntax like C<[1, 2, 3] match [x, y, z] in ...>;
ideally we define this in terms of the representational containers for a type,
rather than for the surface type itself. This means we'll want to do some type
of metaprogramming to define C<match>.

The question here is, do we create metaclasses as an indirect byproduct of
alt-inclusion? i.e. if arrays alt-include operators that apply to all values,
is this sufficient to do OOP?

One issue here is that we don't really have a way to do polymorphism with this
strategy. It might not be a problem if we encode an abstract op against a value
though. Technically that's a more accurate representation: monomorphic ops can
be inlined, whereas polymorphic ops require a runtime decision.


=head1 Type propagation
If we have a list of numbers, say C<[1,2,3]>, we can probably say
C<[1,2,3] + 1> to distribute. This requires the list to do some type inference
and runtime-delegate to the items.


=head1 Interactive parse states
Right now, we have a parser that converts strings to values directly (and
mostly works; see above note about operator precedence). But it would be great
if we had parsers that operated against live editor states so we could get
feedback per keystroke. I think this involves two things:

1. Parsers need to return results that track their original positions and parse
   states. In other words, we need to defer value extraction and have the parse
   step be itself lossless.

2. Parse states need to support early-exit so we can provide documentation and
   completions. This is mainly an issue for typing as it's happening, for
   instance for incomplete constructs.

This forces some things about how we treat continuations. For example, suppose
we're typing C<[1, 2, |>, where C<|> is the edit point. The obvious
continuation is to assume we'll get another C<]> to complete the list. In
parsing terms, we want to both leave an opening at the edit point and consume
future input using a reasonable continuation.

Maybe we just solve this using an early-exit-until-accepting arrangement: the
cursor lets you exit sequences without failing. Maybe the cursor suppresses all
errors; then we'd end up with a true list of alternatives. This is particularly
nice because all states that don't make it up to the cursor are implicitly
rejected.

Q: at what granularity do we memoize? If we have enough alternatives, the memo
table could become huge -- particularly for the continuation.

I think this is easier than I've been assuming. Suppose we have a parse state
that knows where the cursor is; then the moment we've parsed beyond it, any
call to fail() will return a successful parse with a cursor-inside indicator.
