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


=head2 Bootup strategy
How we get to the endpoint isn't the same as which endpoint we're going for; as
a result, the objects we use to boot phi aren't directly related to the set of
objects we ultimately want.

More specifically, phi ends up being a series of class and bytecode objects that
are here-linked to each other. We have three ways to get the bytecode:

1. Hand-write using C<bin>
2. Use Perl abstraction to compile it
3. Generate method calls to phi objects to ask them to generate it

Of those options, the latter gives us the most reusable leverage. So we need to
hand-write enough stuff to get phi objects that can compile other phi objects.
...and that, of course, means that we need to specify what phi is compiling.


=head3 phi bytecode gen
phi classes do two things: they generate instances, and they generate code to
interact with those instances. So let's suppose we have something simple like a
string type that expects to be addressed as a reference:

  class string*
  {
    // Instance state:
    //   uint32 size;
    //   byte   data[size];

    string* +(string *rhs);
    int     size();
    byte*   data();
  }

We can ask it to generate code for us by constructing a class compiler:

  lit(string*)                          # class_object
  .compiler                             # compiler_object
  .size                                 # size_code

The resulting code object implements the C<.size()> method call against an
instance of C<string*> on the stack (i.e. the pointer is on the stack, not the
instance itself).


=head3 Classes, frames, and GC atomicity
TODO

=cut


use constant interpreter_protocol => phi::protocol->new('interpreter',
  qw/ heap_allocate
      unmap_heap
      map_heap
      print_char
      print_string
      exit /);

use constant byte_string_protocol => phi::protocol->new('byte_string',
  qw/ +
      data
      size /);


1;
