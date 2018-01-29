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

use Exporter qw/import/;

use phiboot;
use phibootmacros;

our @EXPORT =
our @EXPORT_OK = qw/mcall make_type mktype bind/;


=head2 Type constructor
Builds a type from a list of method definitions.
=cut

use constant make_type => l                 # [mlist]
  l(resolvercode, i_eval, i_eval), swons,   # [[mlist] resolver]
  lit i_eval, i_cons,                       # [. [mlist] resolver]
  l(tail, head, head, lit i_quote,
    i_cons, swons, swap), i_cons,           # [l . [mlist] resolver]
  lit i_quote, i_cons;                      # [i> l . [mlist] resolver]


sub mcall($)  { (lit psym shift, swap, i_eval) }
sub mktype(@) { le l(@_), make_type, i_eval }
sub bind      { pcons psym shift, l(@_) }


1;
