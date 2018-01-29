=head1 phi infix syntax
This builds on phiapplicative by defining contexts that provide infix operators
for values. The most useful of these is C<any>, which applies to values of any
type.

=head2 Contexts, in general
A context is a parse-time type. This is distinct from runtime types, which is
something I'll need to explain a bit.

Let's pretend we're in C for a minute. Types in C are for the programmer, not
for the value: everything's really just bytes. C can do some type inference but
ultimately has no idea what you're doing under the hood, and types vanish during
compilation. That's the equivalent of phi's parse-time type system. It's
basically a very powerful syntax macro system that integrates with the parser.

Contrast this with a dynamic language like Java or Ruby, in which most method
calls are virtual and objects know what type they are. This is a runtime type,
which is what OOP tends to be about. phi also supports this using self-aware
closures (see C<phiobj.pm> for details). These can be eliminated in some cases,
particularly when the type is constant at runtime.

=head2 Contexts, from a parsing perspective
C<phiapplicative.pm> left a few loose ends, one of them being the persistent
type-tagging of every abstract value. For example, if we're parsing a function
like C<< x -> x + 1 >>, the C<x> in the RHS is encoded as C<[nil 1 get]>.
Obviously C<nil> isn't part of the code we run to retrieve the value. C<nil> is
the context object.

Context objects tie up the other loose end, "parse continuations". Whenever we
parse a value, we immediately ask its declared context for its parse
continuation and then flatmap that onto the parser. This mechanism enables
pretty much every infix and postfix operator in phi. Let's get into the
mechanics of this.

=head3 Specifics of parse continuations
Let's walk through a parse of C<x + 1>, assuming C<x> belongs to the C<any>
context. Suppose we've just parsed C<x>:

  x| + 1                # returns [any 1 get]

Now we ask C<any> for its parse continuation using a C<flatmap> parser. We don't
have a surrounding operator precedence, so we pass in nil as the precedence
floor (I'll explain this more in a minute).

All of this stuff happens within the expression parser:

  [atom] ['combine ...] [[] 'parse-continuation any .] flatmap

Let's break this down piece by piece.

C<[atom]> parses something without operators. It's probably going to be a
variable, a literal, or a paren group.

C<['combine ...]> is part of the combiner function. The C<flatmap> parser passes
it two arguments, the output from the first parser and the output from the
second. In this case, we need the first parser result's type because that's what
implements the C<'combine> method. The second parser's result is probably a list
of C<[operator value]>; most likely it came from a C<seq> of a binary op and
another value. There's more to be said about how this works, but first let's
talk about the rest of the flatmap.

C<[[] 'parse-continuation any .]> generates the continuation parser for the
current value, but this isn't how this function really works. For one thing, we
don't know the atom's type up front, so C<any> wouldn't be hard-coded here. The
other difference is that although C<[]> is on the list, it also isn't hard-coded
into the grammar. These C<flatmap> calls are generated rather than literal.

=head3 Specifics of operator precedence
Ok, in the discussion above I casually mentioned things about generating calls
to C<flatmap>. It turns out that we have to do this, and for an interesting
reason. Here's an example to demonstrate:

  x + |y * z          # * should bind first, so y owns that op
  x * |y + z          # (x * y) needs to bind before we continue

The first example works in a precedence-free world because C<y> is already set
to own the parse with its continuation. That's not very interesting.

In the second example, though, we need C<y> to refuse to parse C<+ z>. This is
where the until-now nil argument to C<parse-continuation> comes into play. When
C<x> produces its parse continuation and sees C<*>, it parses another expression
with a precedence floor that indicates the lowest-precedence operator it's
allowed to continue into. So in call terms we have this:

  # assume x = [any 1 get], y = [any 2 get], z = [any 3 get]
  [atom] ['combine any .] [[] 'parse-continuation any .] flatmap    # parse x

Then C<x *> produces a parser for its RHS like this:

  [atom] ['combine any .] ["*" 'parse-continuation any .] flatmap   # parse y

C<any>'s implementation of C<parse-continuation> will filter the operator list
to just take the ones whose precedence is higher than C<*>.

=cut


1;
