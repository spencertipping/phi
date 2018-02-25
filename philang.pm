=head1 phi language elements
This is where we start to define phi as a language, not just an execution model.
In particular, we need a syntax and things like an editor frontend. This module
gets us there albeit in a circuitous way. (See C<dev/frontend.md> for details.)

The first thing we need is the notion of an abstract value. This is similar to
an AST node but is more aware of its runtime role; in particular, abstract
values constant-fold at parse time and then locally alter the grammar of the
language by specifying their own suffix parsers. Local variables are
dereferenced at parse time.

=head2 Background: stepping a parser
Parsing expression grammars are usually an all-or-nothing proposition, but
that's only true because most implementations rely on a linear call stack to
save temporary state and most hosting languages don't implement resumable
continuations. Absent those constraints, nothing prevents you from having the
parser wait for more input and then keep going when that input is provided.

TODO: elaborate on how/why this matters

=head2 Abstracts, the interpreter, and parse states
The compilation cycle involves a few steps:

  text -> parsed <-> abstracts -> concatenative     # parsing
                  -> abstracts -> concatenative     # optimization
                  -> parsers -> code                # compiler backends

In practice, we can remove the optimization step for reasons I'll explain below:

  text -> parsed <-> abstracts -> concatenative     # parsing
                  -> parsers -> code                # compiler backends

Now we have a simple representational chain without a lot of intervening logic.
This means we can step the two parser stages simultaneously by saving
interpreter states.
=cut

package philang;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philocal;
use phiparse;
use phiobj;

our @EXPORT =
our @EXPORT_OK = qw/ /;


=head2 Primitive abstracts
We need abstract containers for all primitive types.
=cut

use constant cons_type => pmut;
use constant int_type  => pmut;
use constant nil_type  => pmut;
use constant str_type  => pmut;
use constant sym_type  => pmut;
use constant mut_type  => pmut;

use constant abstract_int => mktype
  bind('is-const' => drop, lit 1),
  bind('is-crash' => drop, lit 0);
