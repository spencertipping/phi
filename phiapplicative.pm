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
C<dup> with C<dup 1 +> to fix this:

  x + 1               depth=3 : [3 nil dup 1 + 0 get 1 get +]
  x + 2               depth=6 : [6 nil dup 1 + 3 get 4 get +]
  (x + 1) * (x + 2)   depth=7 : [7 nil dup 1 + 2 get 5 get *]

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
  x + 1               depth=3 : [1 nil dup 0 1 + get 1 get +] : depth=1
  x                   depth=2 : [2 nil 0 get]
  2                   depth=3 : [3 nil drop [2] head]
  x + 2               depth=4 : [2 nil dup 1 + 2 get 3 get +] : depth=2
  (x + 1) * (x + 2)   depth=3 : [3 nil dup 1 + 1 get 2 get *] : depth=1

This, of course, is great because every expression nets exactly one value, so
there's no return value management. The final piece is that C<;> works by
dropping its left argument and keeping its right one; it's no different from any
other binary operator.

=head3 Variables, closures, and capture
The function parser has a nontrivially complex job, the most involved of which
is tracking closure state. Let's talk about how that happens.

  f x xs = map (fn y -> x + y) xs

The closure C<< fn y -> x + y >> will end up being a list, which is convenient
because we have useful identities like argument preloading. For example,
C<[1 +]> works like a closure in that one of the arguments to C<+> is already
supplied. We use the same mechanism to send C<x> into C<fn y> -- so internally,
that function has three args: C<fn x xs y>, two of which are preloaded by
consing their quoted forms onto the list. We don't know which values will be
used by the function, so we pack up all local variables.
=cut
