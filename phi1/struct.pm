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

no warnings 'void';


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

use phi::constQ nil_struct_field_instance => nil_struct_field_class->fn >> heap;


=head3 Struct field types
At a high level there are four different types of fields:

1. C<const> fields, which close over a specific value and are immutable
2. C<fixed> fields: fields whose size is invariant
3. C<array> fields: fields whose size depends on another field's value
4. C<heremarker> fields: fields whose value depends on their struct offset

C<const> must not be physically larger than any other variant because we
sometimes constant-fold a field within a struct by modifying the field object's
type.


=head3 Working with field sizes
As described earlier, not all fields have sizes that are known at compile time.
This poses a problem for struct reductions where we want to get field offsets:
if we hit an unknown size, the API changes from "return the field offset" to
"compile code to produce the field offset".

The simplest workaround for this is to always compile field offsets and sizes.
This simplifies the API at the cost of having foldable strings of
literal/C<iplus> operations in the resulting code. Because those strings are
easy to optimize, that's the strategy I'm going to use here.


=head3 C<const> fields
The simplest in structural terms:

  struct const_field
  {
    hereptr vtable;
    field*  tail;
    string* name;
    *       value;
  };

=cut

use phi::class const_struct_field =>
  maybe_nil_protocol,
  struct_field_protocol,

  "nil?" => bin q{=0 sset01 goto},
  tail => bin q{_=8  iplus m64get_ goto},
  name => bin q{_=16 iplus m64get_ goto},

  size => bin q{                        # asm[&f] self cc
    sset00 _                            # cc asm[&f]
      .drop .lit8 =0_ .l8               # cc asm[0]
    _ goto                              # asm },

  get  => bin q{                        # asm[&f] self cc
    _ =24 iplus m64get                  # asm[&f] cc value
    sget02 .drop .ptr                   # asm cc asm[value]
    sset01 goto                         # asm },

  set  => bin q{                        # asm[v' &f] self cc
    _ .name                             # asm cc name
    "cannot set constant field " .+ i.die },

  fix  => bin q{                        # self cc
    _ .name
    "cannot fix constant field " .+ i.die };


=head3 C<fixed> fields
Fields with a fixed size and specified getter/setter logic.

  struct fixed_field                    # size = 48
  {
    hereptr   vtable;
    field*    tail;
    string*   name;
    int64     size;                     # offset = 24
    bytecode* getter;                   # offset = 32
    bytecode* setter;                   # offset = 40
  };

=cut

use phi::class fixed_struct_field =>
  maybe_nil_protocol,
  struct_field_protocol,

  "nil?" => bin q{=0 sset01 goto},
  tail => bin q{_=8  iplus m64get_ goto},
  name => bin q{_=16 iplus m64get_ goto},

  size => bin q{                        # asm[&f] self cc
    _ =24 iplus m64get                  # asm[&f] cc size
    sget02
      .drop .lit64 _bswap64_ .l64       # asm cc asm[size]
    sset01 goto                         # asm },

  get => bin q{                         # asm[&f] self cc
    _ =32 iplus m64get                  # asm[&f] cc getter
    sget02 .inline                      # asm cc asm[value]
    sset01 goto                         # asm },

  set => bin q{                         # asm[v &f] self cc
    _ =40 iplus m64get                  # asm[v &f] cc setter
    sget02 .inline                      # asm cc asm[]
    sset01 goto                         # asm },

  fix => bin q{                         # v self cc
    # Turn this object into a constant field, destructively.
    _ $const_struct_field_class sget01 m64set   # [.vtable=]
    sget02 sget01 =24 iplus m64set              # [.value=]
    sset01 goto                         # self' };


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
      here_marker
      ptr
      hereptr
      fix /;

use phi::genconst i64_getter => bin q{ asm .m64get .compile .here };
use phi::genconst i64_setter => bin q{ asm .m64set .compile .here };

use phi::class struct =>
  struct_protocol,
  struct_modification_protocol,
  clone_protocol,

  fields => bin q{_=8 iplus m64get_ goto},

  i64 => bin q{                         # name self cc
    sget01 .fields                      # name self cc fs
    =48 i.heap_allocate                 # name self cc fs link
    $fixed_struct_field_class sget01 m64set   # [.vtable=]
    _ sget01 =8 iplus m64set                  # name self cc link [.tail=]
    sget03 sget01 =16 iplus m64set      # [.name=]
    =8 sget01 =24 iplus m64set          # [.size=]
    i64_getter sget01 =32 iplus m64set  # [.getter=]
    i64_setter sget01 =40 iplus m64set  # [.setter=]
    sget02 =8 iplus m64set              # name self cc [self.fields=]
    sset01 _ goto                       # self },

  ptr     => bin q{sget01 m64get :i64 goto},
  hereptr => bin q{sget01 m64get :i64 goto},

  fix => bin q{                         # value name self cc
    sget01 .fields                      # value name self cc fs
    [                                   # v n self cc fs loop
      sget01 .nil?
      [ sget04 "fixing nonexistent field " .+ i.die ]
      [ sget04 sget02 .name .==         # v n self cc fs loop name==?
        [ drop sget04_ .fix             # v n self cc fs'
          drop sset01 sset01 goto ]     # self
        [ _.tail _                      # v n self cc fs.tail loop
          dup goto ]                    # ->loop
        if goto ]                       # self
      if goto ]                         # v n self cc fs loop
    dup goto                            # ->loop },

  get => bin q{                         # asm[struct&] name self cc
    # TODO
    };

use phi::fn struct => bin q{            # cc
  =16 i.heap_allocate                   # cc s
  $struct_class              sget01          m64set   # [.vtable=]
  $nil_struct_field_instance sget01 =8 iplus m64set   # [.fields=]
  _ goto                                # s };


1;
