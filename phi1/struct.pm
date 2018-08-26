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


=head2 Structs
phi structs are basically the same thing as C structs, but with a lot more
options about how memory is used. For example, phi lets you do this:

  struct string
  {
    int32 size;
    int8  data[size];                   # allocated inline
  };

This will result in a struct whose size is dependent on one of its fields.
Instead of getting a constant C<sizeof>, the struct will give you code that
takes a pointer to an instance and returns that instance's size -- same for
field offsets, which means you put arbitrary stuff after C<data> and everything
will continue to work.


=head3 Struct field linkage
It's simpler to cons new links onto the head of a linked list than it is to
maintain a pointer to the end of a list and modify that. Structs, then, are
linked backwards: as you add new fields, you get new links to the "left" of what
exists already. So a struct of C<int x; int y> would be consed with C<y.tail>
set to C<x> and C<x.tail> set to nil.

This layout means our reduce function runs in reverse: the rightmost field is
the first one passed into a reduction.

Struct fields are mutable objects, so we don't cache things about them. This
makes compilation relatively inefficient while runtime is well-optimized by
inlining accessor code directly into bytecode assembler objects.


=head3 Struct field accessors
The C<get> and C<set> operators on struct fields have these signatures:

  asm[      &field] get -> asm[value]
  asm[value &field] set -> asm[]

For example, a field storing a 64-bit int would emit a single C<m64get> as its
getter and C<m64set> as its setter. These accessors are always written into
macro assemblers.
=cut

use phi::protocol struct_field =>
  qw/ tail
      name
      size
      get
      set
      fix /;

use phi::class nil_struct_field =>
  maybe_nil_protocol,
  "nil?" => bin q{=1 sset01 goto};


=head3 Struct field types

=cut


=head3 Struct container object
This is what you'll end up using to manage struct fields. Here's the layout:

  struct struct
  {
    hereptr       vtable;
    struct_field* fields;
  };

=cut

use phi::protocol struct =>
  qw/ get
      set
      offsetof
      sizeof
      fields /;

use phi::protocol struct_modification =>
  qw/ i64
      i32
      i16
      i8
      array
      here_marker
      ptr
      hereptr
      fix /;

use phi::class struct =>
  struct_protocol,
  struct_modification_protocol,
  clone_protocol,

  fields => bin q{_=8 iplus m64get_ goto},

  get => bin q{                         # asm[struct&] name self cc
    # TODO
    };


1;
