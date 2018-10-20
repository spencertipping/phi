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


=head2 Class description
For phi1's purposes, a class is simply an associative list of method hashes and
functions. Class objects are mutable, so they hold a reference to the
associative list and reallocate it when it runs out of capacity. More
specifically:

  struct class
  {
    hereptr<fn> class;
    int         instance_size;
    ptr<fn>     compiled_method_fn;
    int         method_kvs_capacity;
    int         method_kvs_size;
    int         method_kvs_ptr;         # NB: not a pointer; managed manually
  };

=cut

# TODO: write these functions using the phi1 compiler
our %class_methods = (
  class => phi::fn                      # offset self cc
           ->sset->C(1)                 # cc self
           ->swap                       # self cc
           ->go->endfn
);

our $class_class = phi::asm->new('class_class');

$class_class->patch(class_class_fn_hereptr => 8)
            ->Ql(32)
            ->Ql(0)->Ql(0)->Ql(0)
            ->Ql(0);


1;
