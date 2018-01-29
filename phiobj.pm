=head1 Objects and polymorphism in phi
Let's model everyone's favorite object, a 2D point:

  [x y]

Boom. Pure genius right there.

OK, so how do we make this thing self-aware and polymorphic and stuff? We'll
need to affix a type: C<[point-type x y]>. Then methods will look at the type,
do some kind of dispatch, and that becomes a new calling convention. But we can
do even better:

  [[x y] point-type...]

Now we've got more than just a piece of data; we have a fully self-aware
closure. It has some convenient equations:

  obj head = state
  obj tail = type

=head2 Normal calling convention
If you have an object C<obj>, you typically do something like this to interact
with it:

  arg2 arg1 'method obj .

Methods are always symbols by convention. This is important because there are
some situations where the type will need to differentiate between methods and
instance data; it uses primitive types to do this.

=head2 Types
Going back to the 2D point example, what would C<point-type...> actually look
like? It turns out that C<point-type> itself is an object, this time an instance
of a struct type. So a concrete 2D point might look like this:

  [ [3 4] [x y] struct-type... ]

C<struct-type> provides accessors to point fields:

  'x   'get [ [3 4] [x y] struct-type... ] .
  5 'x 'set [ [3 4] [x y] struct-type... ] .

=head3 Self reference
Objects in phi aren't mutable, so any mutators will return modified copies. This
means types will need a way to refer to themselves so they can cons modified
instance state back onto the original type list. Types do this by quoting the
current continuation, which contains the type as the stack top. Here's what that
looks like:

  [ [instance-data...] i> tail head head type... ]

...so in practice, the arguments to the type end up being:

  args... 'method [instance-data...] [type...]

This makes it possible for the type to refer to itself without going through a
global resolver.
=cut

package phiobj;
use strict;
use warnings;

use phiboot;
use phibootmacros;


=head2 Type constructor
Builds a type from a list of method definitions.
=cut

use constant make_type => l                 # [mlist]
  l(resolvercode, i_eval, i_eval), swons,   # [[mlist] resolver]
  lit i_eval, i_cons,                       # [. [mlist] resolver]
  l(tail, head, head, lit i_quote,
    i_cons, swons, swap), i_cons,           # [l . [mlist] resolver]
  lit i_quote, i_cons;                      # [i> l . [mlist] resolver]


=head2 Primitive type wrappers
If we want anything to work with parse contexts, we'll need to wrap them inside
objects. Each of phi's primitive types is wrapped this way.
=cut

sub mktype { le shift, make_type, i_eval }

use constant cons_type_mut => pmut;
use constant nil_type_mut  => pmut;
use constant int_type_mut  => pmut;
use constant sym_type_mut  => pmut;
use constant str_type_mut  => pmut;

use constant cons_type => mktype
  l(pcons(psym '>int' => l drop, lit 0),
    pcons(psym type   => l drop, lit psym 'cons'),
    pcons(psym head   => l head, head),
    pcons(psym tail   => l head, tail, head));

use constant nil_type => mktype
  l(pcons(psym '>int' => l drop, lit 0),
    pcons(psym type   => l drop, lit psym 'nil'));

use constant int_type => mktype
  l(pcons(psym '>int' => l head, head),
    pcons(psym '+' => l i_uncons, head, rot3l, lit psym '>int', swap, i_eval,
                        i_plus, i_cons),
    pcons(psym '*' => l i_uncons, head, rot3l, lit psym '>int', swap, i_eval,
                        i_times, i_cons));

cons_type_mut->set(cons_type);
nil_type_mut->set(nil_type);
int_type_mut->set(int_type);


1;
