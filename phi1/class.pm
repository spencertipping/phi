=head1 License
    phi programming language
    Copyright (C) 2018  Spencer Tipping

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
=cut

package phi;

use strict;
use warnings;
use bytes;


=head1 Classes
First a few definitions:

1. An object is something you hold a pointer to.
2. A class is an object that owns the functionality of other objects.
3. A metaclass is a class whose instances are classes.

phi doesn't support object inheritance natively. This is all managed by
metaclasses, which are responsible for assembling the method lookup functions
used by classes. This is an important point because all objects in phi support a
handful of methods like C<gc_mark> and C<class>, and these methods are managed
by the C<phi_gc_class> metaclass rather than inherited from a base as they would
be in most languages.


=head2 Fixed points between classes and functions
I need to draw this because it involves a few different pieces. First, every
phi1 object (i.e. not an C<int>) begins with a 64-bit pointer to a
method-matching function; I refer to it as the "class fn".

  object = class_fn instance_data...

It's a hereptr, so we can C<call> directly to that destination.

Because it's a hereptr, of course, the fn object itself needs a class fn. So we
can draw a new arrow:

        object = class_fn1 instance_data...
                         |
                         |
                         V
  class_fn2 ... heremark code...
          |              ^
          |              |
          +--------------+

We also have a fixed point for classes, but the circular reference is less
direct because classes are compiled:

                          class_class = class_fn compiled_fn ...
                                               |           |
                                               |           |
                      +------------------------|-----------+
                      |                        |
                      V                        V
     class_class_fn = class_fn_class_fn ... hm code...
                                      |
                                      |
                                      V
        class_fn_fn = class_fn ... hm code...
                             |        ^
                             |        |
                             +--------+

C<class_class> is an object that compiles its own C<class_fn>; in other words,
it's an instance of itself.


=head2 Indirection and mutability
Classes and C<class_fn>s are co-mutable, which means a few things for our
implementation:

1. The class and C<class_fn> both point to the same C<kvs> buffer
2. The class and C<class_fn> both point to each other (they're co-live)
3. C<class_fn>s close over the C<kvs> buffer and size

(3) is sort of expected given that we have the C<mfnd> instruction; here's what
these functions look like specifically:

                                        # mhash cc
  l(kvs) dup l(4) iadd                  # mhash cc kvs &kvs[0]
  swap g32                              # mhash cc &kvs[0] n-kvs
  sget(3) swap                          # mhash cc &kvs[0] mhash n-kvs
  mfnd g64                              # mhash cc mfn
  sset(1) go                            # mfn


=head2 Class description
For phi1's purposes, a class is simply an associative list of method hashes and
functions. Class objects are mutable, so they hold a reference to the
associative list and reallocate it when it runs out of capacity. More
specifically:

  struct class
  {
    hereptr<fn> class_fn;
    ptr<fn>     compiled_method_fn;
    int         method_kvs_capacity;
    int         method_kvs_size;
    int         method_kvs_ptr;         # NB: not a pointer; managed manually
  };

=cut

our $class_class = phi::asm->new('class_class');

$class_class->patch(class_class_fn_hereptr => 8)
            ->Ql(0)->Ql(0)->Ql(0)
            ->Ql(0);


1;
