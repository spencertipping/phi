=head1 Abstract values
phi's concatenative evaluator needs to be as simple as possible not because I
dislike writing interpreter code in Perl (I don't), but rather because the
interpreter itself needs to be very easy to simulate. One such simulation
involves "abstract values", which are collections of constraints that runtime
values will carry. Abstracts are like types but much more specific than most
type systems would support.
=cut

package phiapplicative;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use phiobj;


=head2 What you can do with abstracts
Abstracts give phi programs access to the same kind of API we have in Perl: you
can build an interpreter, set its initial state, and step through it one
instruction at a time. Each time-step is fully inspectable because the
interpreter is just data, and every value's constant/unknown disposition is also
modeled. This is how phi implements constant folding and representational
optimization.

For example, let's suppose we have a function that operates on a pair of
integers and we suspect it allocates unnecessary cons cells in the process. We
can abstract-evaluate it to capture its productive operations, which lets us
build a new, faster function that behaves the same way.

Let's walk through one such function:

  [cons uncons + [] swap cons]

Now let's build an abstract interpreter and step it until it's done:

  'new abstract-interpreter-type .
    'int 'x 'unknown abstract-value-type . 'dpush rot3< .
    'int 'y 'unknown abstract-value-type . 'dpush rot3< .
    [cons uncons + [] swap cons] 'constant abstract-value-type .
      'cpush rot3< .
    run

The result is an abstract interpreter instance that you can inspect to figure
out what it did with its inputs. In this case (with the previous C<run> output
still on the stack):

  'dpop swap .      # -> instance of abstract-value-type

Abstract values can tell you a number of things including their type, how
specified they are, and if they're fully specified, what that value is.
=cut


=head2 C<abstract-value-type>
This comes in a few varieties:

1. C<abstract-constant>: a fully-specified abstract
2. C<abstract-union>: we don't know which of these it is
3. C<abstract-typed-unknown>: we know the type but not the value
4. C<abstract-unknown>: we don't know anything about the value

It's important to note that "type" here refers to primitive,
interpreter-provided types like C<int>, C<symbol>, etc -- we're not talking
about higher-order OOP types.

Another point is that constant-ness is a single-layer thing because it needs to
be. That is, a constant cons cell can contain an unknown. For example, suppose
we're simulating this function on a stack of C<x y>:

  [[] swap cons swap cons]

The output should look like this:

  constant cons(unknown x,
                constant cons(unknown y,
                              constant nil))

Because C<constant cons> is fully specified, we can constant-fold any C<uncons>
operations against those objects.
=cut


1;
