=head1 Applicative grammar for phi
Writing concatenative code is miserable. I'd much rather use something with
ocaml or python syntax than have to keep track of the stack state all the time.
So I'm going to write a self-hosting grammar for phi that compiles applicative
to concatenative code.

=head2 How this works
Rather than doing a direct compilation (which is complex), we can build an infix
-> prefix parser setup and a small executor to evaluate expressions in this
framework. All of that abstraction should be flattened through constant folding
when we compile to a backend.

Interestingly, the VM that executes the prefix notation works exactly like phi
itself does, but relies on structural parsers and is therefore far more
flexible. This is what makes it possible to dispatch on types, and the mechanism
by which methods are lexically, rather than globally, scoped.

I think it's simple: store a stack offset for each abstract value. Then restack
when we need it for concatenative. Use the stack the way it's used in C, except
relative to C<%rsp> not C<%rbp>. This doesn't handle lexical closures, but we
can simply copy the whole stack and send it in, then let the abstract-value
layer sort out which ones actually get used.
=cut

package phiapplicative;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philocal;
use phiparse;

our @EXPORT =
our @EXPORT_OK = qw/ line_comment any_whitespace ignore /;


=head2 Base syntax definitions
Stuff like whitespace, comments, etc. It's worth having these ready.
=cut

use constant line_comment => l
  l(l(pstr "#", phiparse::str, i_eval),
    l(l(pstr "\n", lit 0, phiparse::oneof, i_eval), phiparse::rep, i_eval),
    l(pstr "\n", phiparse::str, i_eval)),
  phiparse::seq, i_eval;

use constant any_whitespace => l
  pstr " \n\r\t", lit 1, phiparse::oneof, i_eval;

use constant ignore => l
  l(drop, pnil),
  l(l(l(line_comment, any_whitespace), phiparse::alt, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


=head2 Expression syntax and parse state
This grammar leverages the parse state more than most. In addition to the
string/offset, we're storing the relative stack depth -- really the number of
expression slots we've allocated. For example, let's go through a simple
function:

  f x = (x + 1) * (x + 2)

C<x> comes in on the stack, so it has position 0 and we begin with a stack depth
of one. If we want to fetch C<x>, we restack C<[depth - position - 1] 0>.
Internally we store a mapping from the name to its abstract, which contains
position, type, and concatenative information:

  x -> [0 nil]

Now let's talk about expression allocation. In total, we have seven in this
function (eight if we count the slot we get from C<x> itself):

  x                   depth=1 : [1 nil 0 get]
  1                   depth=2 : [2 nil drop [1] head]
  x + 1               depth=3 : [3 nil dup 0 get 1 get +]   # BUG
  x                   depth=4 : [4 nil 0 get]
  2                   depth=5 : [5 nil drop [2] head]
  x + 2               depth=6 : [6 nil dup 3 get 4 get +]   # BUG
  (x + 1) * (x + 2)   depth=7 : [7 nil dup 2 get 5 get *]   # BUG
                      depth=8

The concatenative code to fetch a value takes the current stack depth as an
incoming argument; C<depth i get = [depth - i - 1] 0 restack>.

(3), (6), and (7) are broken: C<get> is relative to the I<current> stack depth,
and after a C<dup> we'll have one more entry on the stack. We need to replace
C<dup> with C<dup inc> to fix this:

  x + 1               depth=3 : [3 nil dup inc 0 get 1 get +]
  x + 2               depth=6 : [6 nil dup inc 3 get 4 get +]
  (x + 1) * (x + 2)   depth=7 : [7 nil dup inc 2 get 5 get *]

The end of the function involves one more restack to fetch the returned
expression and reset the stack:

  [0] depth restack

Now we can put all of this together.

=head2 How this maps into parse-land
Conceptually, everything works as above: every value we parse turns into a new
stack entry and we increment the depth in the parse state. In real terms this is
only sometimes true, because the grammar knows some things about linearity. So
let's start there.

If we're parsing an expression like C<x + y>, this would normally net three
stack values: C<x>, C<y>, and C<x + y> -- but obviously C<x> and C<y> are going
to be copies of existing values and don't need to hang around; C<+> is a linear
operator. So the world is a bit better if we can net just C<x + y>. And, of
course, the expression parser has enough information to do exactly that. (The
key is that the two arguments to C<+> must be the top two stack items if we've
done this optimization for subexpressions.) Here's what this new world looks
like:

  f x = (x + 1) * (x + 2)

  x                   depth=1 : [1 nil 0 get]
  1                   depth=2 : [2 nil drop [1] head]
  x + 1               depth=3 : [1 nil dup 0 inc get 1 get +] : depth=1
  x                   depth=2 : [2 nil 0 get]
  2                   depth=3 : [3 nil drop [2] head]
  x + 2               depth=4 : [2 nil dup inc 2 get 3 get +] : depth=2
  (x + 1) * (x + 2)   depth=3 : [3 nil dup inc 1 get 2 get *] : depth=1

This, of course, is great because every expression nets exactly one value, so
there's no return value management. The final piece is that C<;> works by
dropping its left argument and keeping its right one; it's no different from any
other binary operator.

=head3 Variables, closures, and capture
The function parser has a nontrivially complex job, the most involved of which
is tracking closure state. Let's talk about how that happens.

  f x xs = xs.map y -> x + y

The closure C<< y -> x + y >> will end up being a list, which is convenient
because we have useful identities like argument preloading. For example,
C<[1 +]> works like a closure in that one of the arguments to C<+> is already
supplied. We use the same mechanism to send C<x> into C<fn y> -- so internally,
the function becomes C<fn [x] y>: all captured values are passed in a list.

The last piece is that the inner function does two things:

1. Capturable values are rebound to things that unpack the closure list
2. The closure appends a new capture list entry for each I<reference> to a
captured value

Q: can we do some parser magic where the first reference to a captured variable
rebinds it to only generate one closure slot? Of course.

=head3 Types
TODO: what's the strategy here? Values know their types, so this should be
doable at least in theory. The particulars matter, but not right now.


=head2 How this works, concretely
Let's go back to our parsing example, this time as a parse state list:

  ["f x xs = xs.map y -> x + y" 0]

The tail of this, C<[]>, means we have no surrounding lexical scope and no
bindings. It's up to C<f> to push a new lexical context once it's clear we're
defining a function.

From now on I'm going to use C<|> to refer to the parse position within the
string, and leave the offset abstract as C<n>:

  ["|f x xs = xs.map y -> x + y" n]

=head3 Lexical scope encoding
The tail of the parse state contains a stack of lexical scopes, the top of which
is the innermost function we're parsing. For example, if C<f x xs = ...> were
inside another function, we'd have a scope for the parent function and C<f>
would create its own scope to parse the body. That would look something like
this:

  [
    [[] [[xs 0 nil] [x 1 nil]] 2]     # scope inside f x xs = ...
    [[] [[f ...]]              1]     # parent lexical scope
  ]

Let's talk about what these lists are made of. Each one encodes a single scoping
level, which looks like this:

  [closure-captures locals stackdepth]

The parsers themselves manage linear abstracts, and any locals are added to the
C<locals> list. The stack depth is incremented when this happens; we effectively
have a reserved stack slot for the value.

=head3 Initial parse state for the global scope
phi doesn't have a "global scope" per se. Each file or compilation unit or
whatever is considered to have a local scope, and those local scopes can be
returned/chained between files or modified in first-class ways. (This leverage
comes from the deep connection between parsers and types.)

Anyway, this is a reasonable parse state tail to start with:

  [[] [] 1]

Any lexical scope will have a depth of 1 to accept the list of captured closure
variables, although often this list will be nil.

=head3 The destructuring bind
C<f> is parsed as a symbol and doesn't resolve to anything, so we bind it to an
"unbound symbol" value. Its parse continuation provides a few alternatives:

1. C<< ":expr" >>: become a qualified lvalue
2. C<< "lvalue* -> expr" >>: create a lambda
3. C<< "lvalue+ = expr" >>: define a named function
4. C<< "= expr" >>: define a local

Cases (2), (3), and (4) all create a closure scope.

In this case we take (3), which parses everything else. Let's break into the
parse just after C<=>; at this point the LHS has pushed a new closure layer,
which consists of the capture list, the list of locals, and the stack depth:

  [[] [[x nil 1 get] [xs nil 2 get]] 3]

Now the parse state is:

  ["f x xs = |xs.map y -> x + y" n [[] [[x nil 1 get] [xs nil 2 get]] 3]
                                   [[] []                             1]]

=head3 C<xs.map>
C<xs> is parsed as a symbol and matched to stack position 1, so we generate the
description of the abstract value and push a stack entry:

  [[] [[x nil 0 get] [xs nil 1 get]] 3]

We don't see C<xs> in the parse state yet because it's being stored by the
symbol parser; its return value is the abstract:

  [2 nil 1 get]

This value doesn't have a specific type, but it does have a parse continuation
that consumes C<.map> and returns a new abstract. Here's what that looks like:

  ["f x xs = xs.map |y -> x + y" n [[] [[x nil 1 get] [xs nil 2 get]] 4]
                                   [[] []                             1]]

  parse("xs.map") = [2 nil [1 get] . 'map method]

The method parser knows that its LHS is linear, so there's no need to C<dup inc>
the stack depth.

=head3 C<< y -> x + y >>
Aw yeah, time for the fun part. C<y> is parsed as an unbound symbol, which gives
us the alternatives we had for C<f>. This time we take case (2), at which point
we have a new sub-scope:

  ["f x xs = xs.map y -> |x + y" n
      [[] [[y nil 1 get]]                2]     # innermost scope
      [[] [[x nil 1 get] [xs nil 2 get]] 4]     # scope of f
      [[] []                             1]]    # "global" scope

OK, right off the bat we refer to C<x>, which is only bound in the parent scope.
Let's walk through that process in some detail.

First, we fail to find C<x> in the immediate scope, so we begin looking upwards
in the lexical chain. There are two possibilities: either it's defined somewhere
and can be dereferenced, or it's an unbound symbol. In this case it exists one
scope up.

Once we find the reference, we then "pull" it downwards into the current scope.
That involves pulling it into every parent scope until we find it bound locally;
this is important for multi-level closure nesting, e.g. something like this:

  f x = y -> z -> x + y + z

The inner closure references a variable two scopes up, so in reality the C<y>
function closes over C<x> -- even though the only purpose of that closure is to
then forward C<x> to C<< z -> ... >>.

Anyway, the parse state after we've pulled C<x> is this:

  [ [[[1 get]] [[y nil 1 get] [x nil 0 get 0 revnth]] 2]
    [[]        [[x nil 1 get] [xs nil 2 get]]         4]
    [[]        []                                     1] ]

A couple of important points:

1. C<revnth> is C<nth> in reverse: "nth from the end"
2. We bind C<x> as a local once we capture it

(1) matters because once we generate a local entry for a variable, we can't
change anything about how we get to it; and since we cons new variables onto the
beginning of the closure capture list, this means positional references need to
be relative to the end.

(2) is a nice optimization we can make to prevent the same closure variable from
cluttering up the capture space if we refer to it multiple times. Now that C<x>
is a local binding, any further references to it will just generate the stored
code C<depth 0 get 0 revnth>.

=head3 OK, so what about unbound symbols?
Unbound symbols don't work like captured variables because there's no capture
happening: they're literals rather than references. This is a parse-level
distinction. The parser that handles pulldown is gated on finding the value in
the scope chain somewhere; if that parser fails, then we parse an unbound symbol
literal.
=cut
