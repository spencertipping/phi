=head1 List functions for phi
The usual suspects, all implemented here. I was hoping to avoid too much
base-layer code, but it's a lot of work to get to a boot language.
=cut

package philist;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;

our @EXPORT =
our @EXPORT_OK =
  qw/ rev /;


# rev: list reverse
use phi rev1_mut => pmut;
use phi rev1 => l                       # xs r
  swap, dup, nilp,                      # r xs xs.nil?
    l(drop),                            # r
    l(i_uncons,                         # r xs' x
      stack(3, 0, 2, 1),                # xs' r x
      i_cons, rev1_mut, i_eval),        # xs' x:r' rev'
    if_;

rev1_mut->set(rev1);

use phi rev => l                        # xs
  pnil, rev1, i_eval;                   # xs [] rev'



