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

  struct asm[      &struct offset] get -> asm[value]
  struct asm[value &struct offset] set -> asm[]

For example, a field storing a 64-bit int would emit C<iplus m64get> as its
getter and C<iplus m64set> as its setter. These accessors are always written
into macro assemblers; there's no interpreted introspection.
=cut

use phi::protocol struct_const_field =>
  qw/ cvalue
      csize /;

use phi::protocol struct_field =>
  qw/ tail
      name
      constant?
      constant_size?
      size
      get
      set
      fix /;

use phi::class nil_struct_field =>
  struct_const_field_protocol,
  maybe_nil_protocol,
  clone_protocol,

  csize  => bin q{=0 sset01 goto},
  cvalue => bin q{=0 sset01 goto},
  clone  => bin q{goto},
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
  clone_protocol,
  struct_const_field_protocol,
  struct_field_protocol,

  "nil?"           => bin q{=0 sset01 goto},
  "constant?"      => bin q{=1 sset01 goto},
  "constant_size?" => bin q{=1 sset01 goto},
  csize            => bin q{=0 sset01 goto},

  tail   => bin q{_=8  iplus m64get_ goto},
  name   => bin q{_=16 iplus m64get_ goto},
  cvalue => bin q{_=24 iplus m64get_ goto},

  clone => bin q{                       # self cc
    =32 i.heap_allocate                 # self cc new
    sget02 sget01 =32 memcpy            # [new=self]
    sget02 .tail .clone sget01 =8 iplus m64set    # [.tail=]
    sset01 goto                         # new },

  size => bin q{                        # struct asm[&struct offset] self cc
    sset00 _                            # struct cc asm[&s o]
      .drop .drop .lit8 .0              # struct cc asm[0]
    sset01 goto                         # asm },

  get  => bin q{                        # struct asm[&struct offset] self cc
    _ =24 iplus m64get                  # struct asm[&s o] cc value
    sget02 .drop .drop .ptr             # struct asm cc asm[value]
    sset02 sset00 goto                  # asm },

  set  => bin q{                        # struct asm[v' &s o] self cc
    _.name "can't set constant " .+     # struct asm[v' &s o] cc msg
    sget02 .ptr                         # struct asm cc asm[v' &s o msg]
      .get_interpptr .'die              # struct asm cc asm[v' &s o msg i.die]
    sset02 sset00 goto                  # asm },

  fix  => bin q{                        # v self cc
    sget02 sget02 =24 iplus m64set      # v self cc [self.value=v]
    sset01 _ goto                       # self };


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
  clone_protocol,
  struct_const_field_protocol,
  struct_field_protocol,

  "nil?"           => bin q{=0 sset01 goto},
  "constant?"      => bin q{=0 sset01 goto},
  "constant_size?" => bin q{=1 sset01 goto},

  tail   => bin q{_=8  iplus m64get_ goto},
  name   => bin q{_=16 iplus m64get_ goto},
  csize  => bin q{_=24 iplus m64get_ goto},
  cvalue => bin q{
    _.name
    " field has no constant value"_ .+ i.die},

  clone => bin q{                       # self cc
    =48 i.heap_allocate                 # self cc new
    sget02 sget01 =48 memcpy            # [new=self]
    sget02 .tail .clone sget01 =8 iplus m64set    # [.tail=]
    sset01 goto                         # new },

  size => bin q{                        # struct asm[&struct offset] self cc
    _ =24 iplus m64get                  # struct asm[&struct offset] cc size
    sget02
      .drop .drop                       # struct asm cc asm[]
      .lit64 _bswap64_ .l64             # struct asm cc asm[size]
    sset02 sset00 goto                  # asm },

  get => bin q{                         # struct asm[&struct offset] self cc
    _ =32 iplus m64get                  # struct asm[&struct offset] cc getter
    sget02 .inline                      # struct asm cc asm[value]
    sset02 sset00 goto                  # asm },

  set => bin q{                         # struct asm[v &s o] self cc
    _ =40 iplus m64get                  # struct asm[v &s o] cc setter
    sget02 .inline                      # struct asm cc asm[]
    sset02 sset00 goto                  # asm },

  fix => bin q{                         # v self cc
    # Turn this link into a constant field, destructively.
    _ $const_struct_field_class sget01 m64set   # [.vtable=]
    sget02 sget01 =24 iplus m64set              # [.value=]
    sset01 goto                         # self' };


=head3 C<here_marker> fields
These are very simple and can't be constant-folded. They're basically just
uint16 values containing the offset immediately to their right; for example:

  struct has_a_here_marker
  {
    int32 x;                    // offset=0, size=4
    int32 y;                    // offset=4, size=4
    here_marker foo;            // offset=8, size=2, value=10
    int32 z;                    // offset=10, size=4
  };

Here's the struct:

  struct here_marker_field
  {
    hereptr vtable;
    field*  tail;
    string* name;
  };

=cut

use phi::class here_marker_struct_field =>
  maybe_nil_protocol,
  clone_protocol,
  struct_const_field_protocol,
  struct_field_protocol,

  "nil?"           => bin q{=0 sset01 goto},
  "constant?"      => bin q{=0 sset01 goto},
  "constant_size?" => bin q{=1 sset01 goto},

  tail  => bin q{_=8  iplus m64get_ goto},
  name  => bin q{_=16 iplus m64get_ goto},
  csize => bin q{=2 sset01 goto},

  cvalue => bin q{
    _.name
    " field has no constant value"_ .+ i.die},

  clone => bin q{                       # self cc
    =24 i.heap_allocate                 # self cc new
    sget02 sget01 =24 memcpy            # [new=self]
    sget02 .tail .clone sget01 =8 iplus m64set    # [.tail=]
    sset01 goto                         # new },

  size => bin q{                        # struct asm[&struct offset] self cc
    sset02 drop                         # cc asm[&struct offset]
      .drop .drop                       # cc asm[]
      .lit8 =2_ .l8                     # cc asm[2]
    _ goto                              # asm },

  get => bin q{                         # struct asm[&struct offset] self cc
    sset02 drop                         # cc asm[&struct offset]
      .swap .drop                       # cc asm[offset]
      .lit8 .2 .iplus                   # cc asm[value]
    _ goto                              # asm },

  set => bin q{                         # struct asm[v &s o] self cc
    # Ignore the incoming value and set the here marker to its own offset + 2.
    sget02
      .dup .lit8 .2 .iplus              # [v &s o o+2]
      =2_ .sget =2_ .sget .iplus        # [v &s o o+2 &s+o]
      .m16set                           # [v &s o]
      .drop .drop .drop                 # []
    sset02 drop _ goto                  # asm },

  fix => bin q{                         # v self cc
    _.name
    "can't fix here_marker field " .+ i.die };


=head3 Array struct field
An array field stores some amount of data opaquely, repeated by the contents of
another named field. The other named field needs to occur prior to this one in
the struct; otherwise we won't be able to retrieve the struct's size.

WARNING: in practice, arrays and garbage collection don't mix well. For
instance, you can't use a pointer-to-an-element to GC-mark the array; how does
the element know it's part of the array at all? Nor can every element be a
C<hereptr>; we'd quickly overrun the unsigned 16-bit capacity for even
moderately-sized arrays. These issues come up more in frontend designs than
here, but just be aware that it's easy to break GC atomicity with arrays.

Here's the struct:

  struct array_field
  {
    hereptr vtable;
    field*  tail;
    string* name;
    string* n_field;
    int64   element_size;
  };

=cut

use phi::class array_struct_field =>
  maybe_nil_protocol,
  clone_protocol,
  struct_field_protocol,

  "nil?"           => bin q{=0 sset01 goto},
  "constant?"      => bin q{=0 sset01 goto},
  "constant_size?" => bin q{=0 sset01 goto},

  tail   => bin q{_=8  iplus m64get_ goto},
  name   => bin q{_=16 iplus m64get_ goto},

  clone => bin q{                       # self cc
    =40 i.heap_allocate                 # self cc new
    sget02 sget01 =40 memcpy            # [new=self]
    sget02 .tail .clone sget01 =8 iplus m64set    # [.tail=]
    sset01 goto                         # new },

  size => bin q{                        # struct asm[&struct offset] self cc
    sget01 =24 iplus m64get             # s asm self cc nf
    sget03 .drop _                      # s asm self cc asm[&s] nf
    sget05 .get                         # s asm self cc asm[s.nf]
    sget02 =32 iplus m64get bswap64 _   # s asm self cc esize asm[s.nf]
      .lit64 .l64 .itimes               # s asm self cc asm[s.nf*esize]
    sset03 sset01 drop goto             # asm },

  get => bin q{                         # struct asm[&struct offset] self cc
    sset02 drop .iplus _ goto           # asm[&struct+offset] },

  set => bin q{                         # struct asm[v &s o] self cc
    _.name "can't set array field " .+  # struct asm[v &s o] cc msg
    sget02 .ptr                         # struct asm cc asm[v' &f msg]
      .get_interpptr .'die              # struct asm cc asm[v' &f msg i.die]
    sset02 sset00 goto                  # asm },

  fix => bin q{                         # v self cc
    _.name
    "can't fix array field " .+ i.die };


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
      constant?
      constant_size?
      csize
      fields /;

use phi::protocol struct_modification =>
  qw/ i64
      array
      here_marker
      ptr
      hereptr
      fix /;

use phi::genconst i64_getter => bin q{ asm .iplus .m64get .compile };
use phi::genconst i64_setter => bin q{ asm .iplus .m64set .compile };

use phi::class struct =>
  struct_protocol,
  struct_modification_protocol,
  list_protocol,
  map_protocol,
  clone_protocol,

  fields => bin q{_=8 iplus m64get_ goto},

  clone => bin q{                       # self cc
    =16 i.heap_allocate                 # self cc new
    sget02 sget01 =16 memcpy            # [new=self]
    sget02 .fields .clone sget01 =8 iplus m64set    # [.fields=]
    sset01 goto                         # new },

  "constant?" => bin q{                 # self cc
    _ =1_                               # cc 1 self
    [ sget02 .constant?                 # f 1 cc c?
      dup inot                          # f 1 cc c? !c?
      sset02 sset02 goto ]              # c? exit?=!c?
    _ .reduce                           # cc const?
    _ goto                              # const? },

  "constant_size?" => bin q{            # self cc
    _ =1_                               # cc 1 self
    [ sget02 .constant_size?            # f 1 cc c?
      dup inot                          # f 1 cc c? !c?
      sset02 sset02 goto ]              # c? exit?=!c?
    _ .reduce                           # cc constant_size?
    _ goto                              # constant_size? },

  csize => bin q{                       # self cc
    _ =0_                               # cc 0 self
    [ sget02 .csize sget02 iplus        # f s cc s'
      sset02 =0 sset01 goto ]           # s' exit?=0
    _ .reduce                           # cc csize
    _ goto                              # csize },

  "key==_fn" => bin q{$strcmp_fn sset01 goto},
  keys       => bin q{goto},
  kv_pairs   => bin q{_.fields _ goto},
  "{}"       => bin q{                  # name self cc
    _.fields                            # name cc fs
    [ sget01 .nil?                      # name cc fs loop fs.nil?
      [ sget03 "nonexistent field " .+ i.die ]
      [ sget03 sget02 .name .==         # n cc fs loop name==?
        [ drop sset01 goto ]            # fs
        [ _.tail _ dup goto ]           # ->loop(fs=fs.tail)
        if goto ]                       # fs
      if goto ]                         # v n cc fs loop
    dup goto                            # ->loop },

  length => bin q{                      # self cc
    _.fields                            # cc fs
    =0                                  # cc fs l
    [ sget02 .nil?                      # cc fs l loop .nil?
      [ drop sset00 _ goto ]            # l
      [ _ =1 iplus _                    # cc fs l+1 loop
        sget02 .tail sset02             # cc fs.tail l+1 loop
        dup goto ]                      # ->loop
      if goto ]                         # cc fs l loop
    dup goto                            # ->loop },

  reduce => bin q{                      # x0 f self cc
    _.fields                            # x0 f cc fs
    sget03 sget03                       # x0 f cc fs x0 f
    [ sget04 .nil?                      # x x0 f loop cc x.nil?
      [ sset03 drop drop _              # x0 cc
        =0_ goto ]                      # x0 exit?=0
      [ sget04 .tail                    # x x0 f loop cc x'
        sget04 sget04 sget04            # x x0 f loop cc x' x0 f loop
        dup call                        # x x0 f loop cc x1 exit?
        [ sset04 sset02                 # x1 cc f loop
          drop drop =1_ goto ]          # x1 exit?=1
        [ sset03 sset00 _ goto ]        # ->f(x x1 cc)
        if goto ]                       # x1' exit?
      if goto ]                         # x0 f cc x=fs x0 f loop
    dup call                            # x0 f cc xr exit?
    drop sset02 sset00 goto             # xr },

  "[]" => bin q{                        # i self cc
    sget01 .length                      # i self cc n
    sget03 ineg iplus                   # i self cc n-i
    =1 ineg iplus                       # i self cc i'=n-i-1
    sget02 .fields                      # i self cc i' fs
    [ sget02                            # i self cc i' fs loop i'?
      [ _.tail _                        # i self cc i' fs.tail loop
        sget02 =1 ineg iplus sset02     # i self cc i'-1 fs.tail loop
        dup goto ]                      # ->loop
      [ drop sset03 drop sset00 goto ]  # fs
      if goto ]                         # i self cc i' fs loop
    dup goto                            # ->loop },

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

  here_marker => bin q{                 # name self cc
    sget01 .fields                      # name self cc fs
    =24 i.heap_allocate                 # name self cc fs link
    $here_marker_struct_field_class sget01 m64set   # [.vtable=]
    _ sget01 =8 iplus m64set                        # name self cc link [.tail=]
    sget03 sget01 =16 iplus m64set      # [.name=]
    sget02 =8 iplus m64set              # name self cc [self.fields=]
    sset01 _ goto                       # self },

  array => bin q{                       # esize nfield name self cc
    sget01 .fields                      # esize nf name self cc fs
    =40 i.heap_allocate                 # esize nf name self cc fs link
    $array_struct_field_class sget01 m64set     # [.vtable=]
    _ sget01 =8 iplus m64set            # esize nf name self cc link [.tail=]
    sget03 sget01 =16 iplus m64set      # [.name=]
    sget04 sget01 =24 iplus m64set      # [.n_field=]
    sget05 sget01 =32 iplus m64set      # [.esize=]
    sget02 =8 iplus m64set              # _ _ name self cc [self.fields=]
    sset02 sset02 drop goto             # self },

  ptr     => bin q{sget01 m64get :i64 goto},
  hereptr => bin q{sget01 m64get :i64 goto},

  fix => bin q{                         # value name self cc
    sget02 sget02 .fields .{}           # value name self cc f
    sget04_ .fix                        # value name self cc f'
    drop sset01 sset01 goto             # self },

  offsetof => bin q{                    # asm[&s] name self cc
    sget03 .lit8 .0                     # asm name self cc asm[&s 0]
    sget03 ::                           # asm name self cc name::asm
    sget02 ::                           # asm name self cc self::(name::asm)
    [ _ dup .tail .tail sget01          # f cc cons asm cons
            .tail .head                 # f cc cons asm name
      sget04 .name .==                  # f cc cons asm name==?
      [ drop sset01 =1_ goto ]          # cons exit?=1
      [ =1_ .sget =1_ .sget             # f cc cons asm[&s off &s off]
        sget01 .head swap               # f cc cons self asm
        sget04 .size                    # f cc cons asm[&s off size]
        .iplus                          # f cc cons asm[&s off']
        drop sset01 =0_ goto ]          # cons exit?=0
      if goto ]                         # asm name self cc cons fn
    sget03 .reduce                      # asm name self cc cons
    drop sset01 drop goto               # asm },

  sizeof => bin q{                      # asm[&s] self cc
    sget02 .lit8 .0                     # asm self cc asm[&s 0]
    sget02 ::                           # asm self cc self::asm
    [ _ dup .tail                       # f cc cons asm
      =1_ .sget =1_ .sget               # f cc cons asm[&s off &s off]
      sget01 .head _                    # f cc cons self asm
      sget03 .size                      # f cc cons asm[&s off size]
      .iplus                            # f cc cons asm[&s off']
      drop sset01 =0_ goto ]            # cons exit?=0
    sget03 .reduce                      # asm name self cc cons
    drop sset01 drop goto               # asm },

  get => bin q{                         # asm[&struct] name self cc
    # First, convert the struct pointer into a field pointer by adding the field
    # offset. This happens to the assembler object.
    sget03                              # asm name self cc asm[&struct]
    sget03 sget03 .offsetof             # asm name self cc asm[&struct offset]

    # Now retrieve the field in question.
    sget02 _                            # asm name self cc self asm
    sget04 sget04 .{}                   # asm name self cc self asm f
    .get                                # asm name self cc asm
    sset03 sset01 drop goto             # asm },

  set => bin q{                         # asm[v &struct] name self cc
    sget03                              # asm name self cc asm[v &s]
    sget03 sget03 .offsetof             # asm name self cc asm[v &s offset]
    sget02 _
    sget04 sget04 .{}
    .set
    sset03 sset01 drop goto             # asm };

use phi::fn struct => bin q{            # cc
  =16 i.heap_allocate                   # cc s
  $struct_class              sget01          m64set   # [.vtable=]
  $nil_struct_field_instance sget01 =8 iplus m64set   # [.fields=]
  _ goto                                # s };


1;
