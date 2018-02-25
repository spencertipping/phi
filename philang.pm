=head1 phi language elements
This is where we start to define phi as a language, not just an execution model.
In particular, we need a syntax and things like an editor frontend. This module
gets us there albeit in a circuitous way. (See C<dev/frontend.md> for details.)

The first thing we need is the notion of an abstract value. This is similar to
an AST node but is more aware of its runtime role; in particular, abstract
values constant-fold at parse time and then locally alter the grammar of the
language by specifying their own suffix parsers. Local variables are
dereferenced at parse time.

=head2 Abstracts, the interpreter, and parse states
The compilation cycle involves a few steps:

  text -> parsed <-> abstracts -> concatenative     # parsing
                  -> abstracts -> concatenative     # optimization
                  -> parsers -> code                # compiler backends
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
