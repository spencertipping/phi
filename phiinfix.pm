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

=head3 Types of grammars supported by contexts
The goal is to support pretty much any subexpression grammar with
well-integrated operator precedence. For example, let's suppose we have two
contexts: C<all> for the usual suspects, and C<ebnf> for defining parsers. Then
we'll have hybrid expressions:

  parser = ebnf atom | '(' atom ')'

Mechanically, C<ebnf> refers to a value whose parse continuation is an EBNF
expression, but we need to be aware of operator precedence:

  parser = ebnf atom | '(' atom ')', 5.10251
                -------------------  -------
                    ebnf context       any

...and C<ebnf> knows to reject the comma (which might mean something in EBNF
terms) because its surrounding precedence is specified as C<=>, which has higher
precedence. So C<ebnf>'s parse continuation was generated using a method call
like this:

  '= 'parse-continuation ebnf .

Q: is it worth building abstract values at parse-time rather than compiling to
stack accessors? This might give us a lot more flexibility.

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

  [atom] ['combine any .] ['* 'parse-continuation any .] flatmap   # parse y

C<any>'s implementation of C<parse-continuation> will filter the operator list
to just take the ones whose precedence is higher than C<*>. It then forms an
C<alt> of those to parse its immediate continuation. The result is a parser that
will refuse to parse any lower-precedence operators, which will fall back to the
parent parse step, folding a term. This repeats until all higher-precedence
terms are folded, effectively implementing a shunting yard parser.

=head3 The C<'combine> method
Each parser needs to return an abstract value, so C<combine> is really "combine
and compile". For example, C<x + 1> needs to end up returning something like
C<[int dup inc [1 get] . [2 get] . +]>. (In practice it returns method calls
against objects, but that's the idea.)
=cut

package phiinfix;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use phiobj;


=head2 C<infix> context type
This will probably make more sense from the point of view of C<any>, but we need
to write this part first. Basically, this is the type of any context that
provides precedence-based infix operators; the "instance state" is the
precedence/associativity list and a compiler for each operator (or nil, in which
case that operator becomes a symbolic method call).

At a high level we have a few basic methods:

1. C<'op parse-continuation> : a parser
2. C<'op precedence> : an integer
3. C<'op associativity> : C<'left|'right>
4. C<'v1 ['op 'v2] combine> : a value (this is where ops get compiled)

Time to implement these puppies, but first let's talk about the instance state
in detail.

=head3 C<infix> instance state
There are two state elements stored by an infix instance. One is the base atom
parser, and the other is a precedence list. Here's an example precedence list:

  [
    [4 right [** ...]]            # highest precedence
    [3 left [* ...] [/ ...]]
    [2 left [+ ...] [- ...]]
    [1 right [= eq-compiler...]]
  ]

Note that the lists don't have to go in precedence-order.

There's something a little devious going on with C<=> above. C<=> is a special
operator in that it modifies the parse state rather than returning an assignment
value, since that's how phi implements lexical scopes. All of that is great, but
technically the compiler-list only gets the two parse results on the stack; how
does it update the parse state?

C<flatmap> to the rescue! The combiner is at liberty to consume two stack args
and return one, but it is equally at liberty to consume three and return two;
the lower argument is the parse state. So we can do everything we want to from
inside the combiner, meaning that not only can we compile stuff, but we can
modify the scope while we do it.

TODO: I don't think this strategy makes sense yet. It isn't clear who's building
the parsers, and not every operator takes a regular expression as its RHS. This
abstraction isn't quite right.

=head3 Functions

  ps sym  lookup  = match ps with
    | [[s' def...] ps'] -> sym == s' ? [def...] : ps' sym lookup
    | []                -> []

  pss sym  sublist = match pss with
    | ps:pss' -> ps.tail.tail sym lookup ? ps
                                         : pss' sym sublist
    | []      -> []

  pss sym  precedence    = pss sym sublist tail head
  pss sym  associativity = pss sym sublist head

=cut

use constant lookup_mut => pmut;
use constant lookup => l                    # ps sym
  swap, dup, nilp,                          # sym ps <1|0>
    l(swap, drop),                          # []
    l(i_uncons, i_uncons,                   # sym ps' [def...] s
      stack(0, 3), i_symeq,                 # sym ps' [def...] <1|0>
      l(stack(3, 0)),                       # [def...]
      l(drop, swap, lookup_mut, i_eval),
    if_),
  if_;

lookup_mut->set(lookup);


use constant sublist_mut => pmut;
use constant sublist => l                   # pss sym
  swap, dup, nilp,                          # sym pss <1|0>
    l(swap, drop),                          # []
    l(i_uncons, dup, tail, tail,            # sym pss' ps ps.tail.tail
      stack(0, 3), lookup, i_eval, nilp,    # sym pss' ps <1|0>
      l(stack(3, 0)),                       # ps
      l(drop, swap, sublist_mut, i_eval),
    if_),
  if_;

sublist_mut->set(sublist);


=head2 C<any> context
Alright, let's get into this. C<any> is where we start because its connection to
values is deliberately minimal. We have these operators in order of descending
precedence:

  left    prefix'               # quote operator
  left    x[y] (x y) x.method
  right   ::                    # type annotation
  right   **
  right   prefix! prefix~ prefix-
  left    =~ !~
  left    * / % //
  left    + -
  left    << >>
  left    < > <= >=
  left    == != <>
  left    &
  left    | ^
  left    in  not in
  left    is  is not
  right   .                     # cons
  left    ++                    # collection append
  left    &&
  left    ||
  left    .. ...
  right   ?:                    # a bit special from a parsing point of view
  left    ,
  right   =                     # not quite a real operator
  right   not
  left    and
  left    or xor
  left    ;

Now let's get into the mechanics of parsing these things. First up is
associativity: given that we're just passing in the operator we want to parse,
how does the parse continuation know whether it's left or right associative? In
this case the answer is that the continuation parser makes that decision by
either including or excluding same-precedence operators. This makes it possible
for a sub-context to switch the associativity of operators if it wants to.
=cut


1;
