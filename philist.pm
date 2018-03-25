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
  qw/ rev list_length list_append nthlast lget lset /;


# rev: list reverse
use phi rev1_mut => pmut;
use phi rev1 => l                       # xs r
  swap, dup, nilp,                      # r xs xs.nil?
    l(drop),                            # r
    l(i_uncons,                         # r xs' x
      stack(3, 0, 2, 1),                # xs' r x
      i_cons, rev1_mut, i_eval),        # xs' r' rev'
    if_;

rev1_mut->set(rev1);

use phi rev => l                        # xs
  pnil, rev1, i_eval;                   # xs [] rev'


# append: list append
use phi list_append_mut => pmut;
use phi list_append => l                # xs ys
  nip, nilp,                            # xs ys xs.nil?
  l(stack(2, 0)),                       # ys
  l(                                    # xs ys
    swap, i_uncons, rot3r, swap,        # x xs' ys
    list_append_mut, i_eval,            # x xs'++ys
    swons),                             # x::(xs'++ys)
  if_;

list_append_mut->set(list_append);


=head3 C<list-length> function

  []        list-length = 0
  [x xs...] list-length = xs... list-length inc

Derivation:

  xs  dup nilp              = xs <1|0>
  []  drop 0                = 0
  xs  tail list-length inc  = 1 + length(xs.tail)

=cut

use phi list_length_mut => pmut;
use phi list_length => l
  dup, nilp,
    l(drop, lit 0),
    l(tail, list_length_mut, i_eval, lit 1, i_plus),
    if_;

list_length_mut->set(list_length);


=head3 C<nthlast>
Functionally:

  xs i nthlast = xs rev i nth
  xs i nth     = i == 0 ? xs.head : xs.tail i-1 nth

Concatenatively:

  xs i      swap rev swap nth

  xs i      dup if
    xs i    swap tail swap 1 neg + nth
    xs 0    drop head

=cut

use phi nth_mut => pmut;
use phi nth => l
  dup,
    l(swap, tail, swap, lit 1, i_neg, i_plus, nth_mut, i_eval),
    l(drop, head),
    if_;

nth_mut->set(nth);

use phi nthlast => l swap, rev, i_eval, swap, nth, i_eval;


=head2 Object state list updates
Hand-writing code to update an object's instance state is awful, so let's write
up some helper functions to get and replace individual list elements:

  xs i   lget = i == 0 ? xs.head   : xs.tail i-1 lget
  xs v i lset = i == 0 ? v:xs.tail : (xs.tail v i-1 lset) xs.head cons

=cut

use phi lget_mut => pmut;
use phi lget => l                       # xs i
  dup,                                  # xs i i
    l(lit 1, i_neg, i_plus, swap, tail, swap, lget_mut, i_eval),
    l(drop, head),
  if_;

lget_mut->set(lget);


use phi lset_mut => pmut;
use phi lset => l                       # xs v i
  dup,                                  # xs v i
    l(lit 1, i_neg, i_plus,             # xs v i-1
      stack(2, 2, 0, 1), tail, swap,    # xs v xs.tail i-1
      rot3l, swap, lset_mut, i_eval,    # xs (...lset)
      swap, head, i_cons),              # xs.head:(...lset)
    l(drop, swap, tail, swons),         # v:xs.tail
  if_;

lset_mut->set(lset);
