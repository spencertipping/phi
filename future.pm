=head1 Delta encoding
Suppose we define parsers in terms of insertions/deletions. Then editor
commands turn into parser deltas, which turn into state deltas, and we have
fully incremental parsing. Each parser works like this:

  parser + insertion -> Δcontinuation

...where Δcontinuation encodes success/failure and the parsers that should pick
up at that point. Parsing is a top-down system; we rely on the call stack to
manage immediate parent/child linkage.

Continuations would be C<([Δpos, parser], ...)>? ...how could this possibly
work?

Scratch that; continuations are just C<Δpos>. Parsers are always managed
top-down.

Parser results don't maintain their own starting positions; this is always
computed top-down as well. They do maintain their own displacements. This way
insertions are efficient. Things like C<seq> are free to (and should) implement
their own offset arrays to speed up searches.


=head1 Parse results
This is handled awkwardly right now. Parse results produce outputs, which
contain values, some of which are arrays of more results.

There are a few potential ways to do better:

1. Use some type of bless-into mechanism for seq arrays
2. Subclass outputs for app-specific purposes
3. Do something clever with function insertion points to get derivative
grammars
4. Provide output subclasses that make things easy

Other ideas that might be worth considering:

1. seq() outputs should be usable as inputs for parsers
2. Have C<result> store the metadata we're currently storing on outputs; then
results provide accessors and could parse on demand.
3. Inputs should provide a hash(start, length) so we can memoize parses. It's
expensive to reparse in astable configurations; ideally we'd have an LRU of
some sort, maybe timed.


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
