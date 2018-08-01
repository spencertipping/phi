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
CTTI handles conditional constructs.

Q: is it possible to use upwards constant propagation or structural parsing to
break through the indirect function call node?


=head3 Operand positioning and GC atomicity
Let's say we're doing something simple like this:

  (foo + 1).bar(bif + baz, [1, 2, 3].map(_ + 1))

Structurally, we're invoking the C<bar> method on C<foo + 1>, passing in two
arguments. The equivalent concatenative code would have roughly this evaluation
ordering and stack behavior:

  const1 sgetXX .+                      # foo+1
  sgetXX sgetXX .+ swap                 # bif+baz foo+1
  $nil lit8+3 ::
       lit8+2 ::
       lit8+1 ::                        # bif+baz foo+1 [1,2,3]
  [ swap const1 swap .+ swap goto ]     # bif+baz foo+1 [1,2,3] _+1
  swap .map                             # bif+baz foo+1 [1,2,3].map
  swap .bar                             # (foo+1).bar(bif+baz,[1,2,3].map)

We have a problem, though: let's suppose C<foo+1> returns a pointer value and
C<bif+baz> allocates memory. If C<bif+baz> kicks off a GC, we'll lose C<foo+1>
because it's an untyped stack entry. Method calls of the form C<a.b(c,...)> need
to store C<a>, C<c>, and all other arguments in the frame somewhere, which means
we need temp slots allocated for them. Then our code looks more like this:

  const1 f.XX .+ f.tmp1=                # stash foo+1
  f.XX f.XX .+ f.tmp2=                  # stash bif+bar
  $nil   lit8+3 :: f.tmp3=
  f.tmp3 lit8+2 :: f.tmp3=
  f.tmp3 lit8+1 :: f.tmp3=              # stash [1,2,3]
  [ swap const1 swap .+ swap goto ] f.tmp4=
  f.tmp4 f.tmp3 .map f.tmp5=            # stash [1,2,3].map
  f.tmp3 f.tmp2 f.tmp1 .bar f.tmp6=     # stash (foo+1).bar(...)

NB: B<getters are destructive>: C<f.tmpX> returns and clears C<tmpX> -- we need
this behavior to avoid falsely pinning references.


=head3 Multi-argument staging and transient pins
Every multary operation needs a transient GC pin for its arguments. As mentioned
above, the pin's duration is precisely constrained: each argument must be pinned
immediately after it's generated, and the pin must end immediately before the
operation itself is made. The event timeline looks like this:

  event                                 pin set

  A = foo + 1                           A
  B = bif + bar                         A B
  C = 3 :: nil                          A B C
  D = 2 :: 3 :: nil                     A B   D
  E = 1 :: 2 :: 3 :: nil                A B     E
  F = [...]                             A B     E F
  G = E.map(F)                          A B         G
  H = A.bar(B, G)                                     H


=head3 Local variable pins
Locals are less transient than linear expressions. Some languages like Java pin
the reference throughout the local scope, whereas CPS-converted languages and
Clojure eliminate the pin as soon as the local is no longer used within its
scope (this as a natural byproduct of CPS conversion, or in Clojure's case a
deliberate effort to clear locals).

Nothing really prevents phi from using CPS internally. Although not all backends
support continuations, they don't have to: phi's stack-vs-heap management of
frames is driven entirely by libraries, and each basic block can be translated
into a native or interpreted. phi owns the stack and we can abstract-trace to
find cases where we're stack-allocating the frames by looking for short
sequences of C<get_stackptr ... set_frameptr>. Those frame slots can then be
aliased to local variables in managed targets.

...anyway, none of this really addresses phi's use of CPS when it comes to
compilation and local variables. Let's get into that.

=head4 Simple CPS conversion
Let's take a function with simple control flow:

  () -> let x = foo() in
        let y = bar() in
        (x + 1).to_s

A normal CPS conversion would produce something like this:

  k ->
    foo (x ->
    bar (y ->
    + x 1 (r ->
    .to_s r k)))

It doesn't look like we get any particular advantage in GC terms because the set
of capturable variables continues until we use C<k> as the return continuation.
But if we're efficient about lambda capture then things become easier; C<y>, for
instance, isn't used at all and never needs to be stored.

=head4 Branched control flow
phi's instruction set doesn't differentiate between values and functions, but
the CPS layer itself needs to be aware of the distinction. For example:

  x -> x < 0 ? foo(0) : x

Here are two functionally equivalent CPS conversions:

  x k ->                        x k ->
    < x 0 (lt0 ->                 < x 0 (lt0 ->
    if lt0                        if lt0
      (() -> foo 0 k)               (x' k' -> foo 0 k')
      (() -> k x)                   (x' k' -> k' x')
      (branch ->                    (branch ->
    branch ()))                   branch x k))

These are equivalent because in concatenative terms we have no closures. Every
lambda expression in these expansions will end up consuming its entire set of
closed-over variables, as the branch does in the right conversion. Ultimately,
then, we'll end up with something like this:

  x k ->
    < x 0 [x k] (x k lt0 ->
    if lt0
      (x k -> foo 0 k)
      (x k -> k x)
      [x k] (x k branch ->
    branch x k))

Since each function is prefixed with the list of values it captures, we have a
list of stack layouts, one per intermediate continuation:

  x k ->                        # x k
    < x 0                       # x k lt0
    if lt0 ...                  # x k branch
    goto                        # ...

Because branch signatures are unified, locals clearing is conservative; we could
have two correlated branches and the second one could falsely (since we know the
outcome already) pin a reference. I think this is ok.

=head4 Parse-time pin set calculation
Parse continuations mirror flow continuations; within a function, we can have a
right-associative C<;> operator and parsers can return pairs of C<ctti, refset>
to indicate their dependencies.

Doing this solves the active-refset problem by automatically returning the
continuation's refset. Then each compilation point has an input and output
refset and we know exactly when to clear any given reference.

Abstract evaluation is owned by CTTIs at this point; local method overloads
should be sufficient for everything except recursion and other very nonlocal
control flow. I'm not 100% convinced we have adequate abstract-eval machinery,
but it's not a bad start.

The last part of this is how we handle local continuations that demand their own
call frames. Tail-recursive loops can be inlined into the parent frame, but any
function being invoked non-tail-recursively will need a separate frame. This is
standard CPS.


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
low-level instructions, and it's a low priority because there are easy
workarounds for now.


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
