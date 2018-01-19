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

Now let's talk about expression allocation. In total, we have six in this
function:

  x                 -> depth=1 : [0 nil]
  1                 -> depth=2 : [1 int]
  (x + 1)           -> depth=3 : [2 nil [0 1] 0 restack +]
  x                 -> depth=3 : [0 nil]        # reference to existing
  2                 -> depth=4 : [3 int]
  (x + 2)           -> depth=5 : [4 nil [0 1] 0 restack +]
  (x + 1) * (x + 2) -> depth=6 : [5 nil [0 2] 0 restack *]

The end of the function involves one more restack to fetch the returned
expression and reset the stack:

  [0] 6 restack

=cut
