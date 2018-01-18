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

TODO: fix this; a prefix->concatenative runtime doesn't substantially simplify
the actual logic of managing stack entries. Are we storing our own binding list
instead?
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
