=head1 License
    phi programming language
    Copyright (C) 2018  Spencer Tipping

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
=cut

package phi;

use strict;
use warnings;

no warnings 'void';


=head2 Immutable CTTI stack assembler
NB: CTTI = compile-time type information, in contrast to RTTI = runtime type
information. In C++, CTTI would be something like C<double> or a non-virtual
class type, whereas RTTI would be a virtual thing. phi's CTTIs are more involved
than just types; you can factor arbitrarily much stuff into a CTTI if you want
to. I'll explain this in a section below.

This is kind of a lot at once, so I should probably explain what we're doing
here and why. Let's start with the frontend.

phi's frontend is built on parsing expression grammars, which consume text and
return values. An important characteristic of PEGs is that they can parse a bit
of stuff, fail out of an alternation, and resume somewhere else; because of
this, they need to _produce_ values rather than modifying them. That requires
incremental assembler updates to be immutable.

The frontend also interpolates parse rules into the grammar depending on the
compile-time type information (CTTI) that's active on the stack at any given
moment. In code terms, this means you can mention a value, placing on the stack
top, and that value immediately has an opportunity to bend the grammar at that
point. Doing this accurately involves tracking the CTTI for all values both on
the stack and bound locally in variables.

There's some backend-focused machinery too. C<bin> supports the C<[ ... ]>
notation, which pushes a reference to an inline function (or more accurately,
any inline binary data at all); our assembler needs something functionally
equivalent so it's reasonably simple to generate things like loops or
if-branches. This raises a couple of questions:

1. Can we use it to generate non-phi stuff like machine code?
2. If we C<call> one of these things, what happens to our CTTI state?

...and this gets us into CTTI-layer functional transforms.


=head3 More about CTTIs in practice
C doesn't support RTTI, so all of its compile-time values are what I'm calling
CTTIs. For example, C<double x> is a CTTI that's as big as a C<double> and that
gives you C<double> functionality. C<+> is polymorphic in C, but the
polymorphism happens at compile-time rather than runtime.

Most languages force an upper bound on how much you can factor into CTTIs. That
is, you can't do stuff like saying "I have an C<int> that I know to be positive"
and then get constant folding when you call C<abs()> on it. phi is different in
that you can encode arbitrarily much stuff about a value into the CTTI if you
want to. CTTIs in phi are more like abstract values than they are like types:
the type is just one dimension of information about a quantity. (And you might
not know the type of a CTTI; phi gives you stuff like C<unknown> to track and
manipulate opaque quantities.)

Anyway, I guess the takeaways here are:

1. "A CTTI" really means "the compile-time type information of a value"
2. A CTTI usually specifies a value's type, but it doesn't have to
3. A CTTI interprets operators, methods, etc, and assembles code for them
4. At compile time, every value is a CTTI
5. ...and that includes pointers, which are user-defined types


=head3 Signatures, types, and evaluation layers
Most languages have a very simple system to infer value types. For example,
suppose I write something like this in C:

  struct foo
  {
    int bif;
  };

  struct foo *gimme_a_struct(void) { ... }

  void bar(void)
  {
    printf("%d\n",
      gimme_a_struct()->bif);           // behold! type system evaluation
  }

I'm being generous about what constitutes "evaluation" here; C's type system
really amounts to "use the return type of a function verbatim". There are some
situations where it has to do more work, though; constructs like C<?:> involve
unifying the types of the two branches. That's why this code works:

  struct foo *f1(void);
  struct foo *f2(void);

  int condition;

  int bar(void)
  {
    return (condition ? f1() : f2())    // <- type inferred as (struct foo*)
           ->bif;
  }

phi doesn't look at this as a syntactic builtin. Instead, C<?:> would be an
operator or interpolated parse rule specified by a CTTI, and it's up to whoever
implements C<?:> (a boolean CTTI most likely) to construct a return-type CTTI
representing the outcome of the conditional. All of this happens at parse time.

...all of which is well and good, but what happens when you've got code like the
above with function calls? We'll need something analogous to C's type signatures
for those functions. And this creates a couple of problems:

1. phi supports parametric function types (like C++ implicit templates)
2. Functions aren't required to advertise signatures

In other words, functions' CTTI manipulation is more or less a free-for-all. And
this is appropriate given that any type system is going to produce edge cases
where we can lose information about values -- particularly when those values are
described by CTTIs with a lot of extra properties or other such complexities.
(Remember that unlike in most languages, phi's CTTIs are a fully open-ended
space.)

This means that a function CTTI is also a function _over_ CTTIs: applying it
will, at compile time, give you a new state containing the CTTIs of the returned
values. Given this utopia of compositional type propagation, why am I mentioning
signatures at all then? You guessed it: recursion.

At runtime, exactly one conditional branch is taken. At compile time, all
branches are taken. This means that functions which are runtime-recursive are
compile-time nonterminating:

  factorial n = n > 0 ? n * factorial(n - 1) : 1

There are actually two very good reasons we can't build a CTTI for C<factorial>
by evaluation alone:

1. It's self-referential, so we'd need some kind of fixed-point solver
2. The recursive reference occurs before we've even parsed the whole function

When you need the equivalent of a forward definition, you can address
C<factorial> as an opaque function with a specific type signature. Then you'll
get a return-type CTTI for free (really, on the promise of your signature) and
everything will work out.

All of this raises the question, what _is_ a function signature? Structurally
it's just any function over CTTIs. The signature of the inner C<factorial> could
be as simple as C<< T -> T >>; all it needs to do is construct a return value
CTTI without referring to itself in the process.

NB: you can easily have CTTIs that represent constant values and behave like
constants -- so there's nothing stopping you from fully evaluating
C<factorial(4)> at compile time, dropping the resulting constant in rather than
emitting a function call. The behavior you'll get depends entirely on how the
CTTI handles conditional constructs. (TODO: technically this is true, but how
would we tell the constant CTTI to break through the opaque function call?)


=head3 Primitives and type annotations
Like C++, phi's compiled code is largely untyped. C<int> may be a CTTI that
provides all kinds of structure around a value, but C<int + int> can be reduced
to a single C<iplus> opcode -- there's no reason for C<int> to establish any
runtime type information that would allow you to reconstruct it from generated
bytecode.

This has two implications. First, there are no "primitive" types: our base CTTIs
like C<int> and C<double> are full classes that happen to back into bytecodes
that implement their functionality. Second, bytecodes are almost entirely
untyped; in CTTI space, C<iplus> has signature C<< ?, ? -> ? >> (not all
bytecodes are this way; C<dup> is typed C<< T -> T, T >>). This means we'll need
a way to specify or modify the CTTI for any given value. CTTI modification is
semantically equivalent to C++'s C<reinterpret_cast>.

NB: every logical value must occupy exactly one stack slot. If you use more than
one slot for a value, things like C<swap> will break horribly. I may add support
for this in phi2; it just requires some CTTI negotiation to emit the right
low-level instructions.


=head3 Assembler structure
Assemblers are linked lists, basically. You start with a nil base link and cons
new stuff onto it, holding onto those new links. These conses proceed
rightwards, so the interpretation is like this:

  (((nil swap) dup) drop)       -> [swap dup drop]

There are several different kinds of assembler links:

1. Low-level stack instruction (like the above)
2. Insert a constant
3. Set CTTI of stack
4. Set CTTI of frame
5. Set CTTI of interpreter

These link types are enough to fully represent the low-level operational
semantics of phi, but they're lossy: just as C<bin> lossily compiles method
calls down to their constituent C<dup m64get ...> operations, the links encoding
a method call don't contain information about which method call was made, or
even that one was made at all. Nor should they.


=head3 Compilation target polymorphism
Not all backends support the same set of bytecode instructions, which means we
need a set of basis CTTIs that we switch out depending on which backend we're
targeting. These CTTIs aren't things like "target Ruby vs Javascript"; they're
more like "target a managed-memory OOP environment vs a flat-memory
environment."


=head3 Parse replay
I mentioned earlier that CTTIs provide the interface to their underlying values,
and that they locally bend the grammar by interpolating parsers. These two
conditions jointly create a problem for backend polymorphism: what happens if
the basis CTTIs interpolate different grammar rules?

Ultimately there's nothing stopping them from doing this, nor should there be.
It's possible that it's appropriate for CTTIs to use a grammar that is covariant
with their compilation target -- presumably it would be syntactically
equivalent, but it might use a different grammar to get there.

I think that's fine. It's a little slow, but memoizing the parse across
different basis CTTI sets creates edge cases that will cause things to fail.
=cut


1;
