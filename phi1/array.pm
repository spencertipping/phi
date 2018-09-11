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


=head2 Direct and indirect arrays
We can get a lot of things done with a couple of multipurpose classes. In C++
terms they look like this:

  struct direct_array<T>
  {
    hereptr class;
    int32_t esize_bits;                 // esize == sizeof(T) * 8
    int32_t n;
    here_marker;
    T       xs[n];                      // pretend you can do this
  };

Direct arrays have their element storage allocated inline with the metadata, so
you can't resize them in place.

Indirect arrays, on the other hand, manage their element allocation externally.
Here's what they look like:

  struct indirect_array<K, V>
  {
    hereptr  class;
    int32_t  ksize_bits;
    int32_t  vsize_bits;
    int32_t  n;
    int32_t  capacity;
    K       *ks;
    V       *vs;
  };

These arrays can be used as associative maps because they contain two element
stores, although C<vs> remains null until you use associative features.
=cut

use phi::protocol array =>
  qw/ n
      &[]
      esize_bits
      size
      data /;

use phi::protocol array_value =>
  qw/ []
      []= /;                            # NB: []= should return prior element

use phi::protocol bitset =>
  qw/ add
      contains? /;


use phi::class direct_array =>
  array_protocol,

  esize_bits => bin q{_ =8  iplus m32get _ goto},
  n          => bin q{_ =12 iplus m32get _ goto},
  data       => bin q{_ =18 iplus        _ goto},

  size => bin q{                        # self cc
    sget01 =8  iplus m32get             # self cc esize_bits
    sget01 =12 iplus m32get itimes      # self cc size_bits
    =7 iplus =3 ishr sset01 goto        # size_bytes },

  # NB: for bitfields, this returns a pointer to the byte that contains the bit.
  "&[]" => bin q{                       # i self cc
    sget01 =18 iplus                    # i self cc &xs[0]
    sget02 =8  iplus m32get             # i self cc &xs[0] esize_bits
    sget04 itimes =3 ishr iplus         # i self cc &xs[i]
    sset02 sset00 goto                  # &xs[i] };


use phi::class i8_direct_array =>
  array_protocol,
  array_value_protocol,

  direct_array_class->methods_except(qw/ &[] size esize_bits /),

  size       => bin q{_ =12 iplus m32get _ goto},
  esize_bits => bin q{=8 sset01 goto},
  "&[]"      => bin q{                  # i self cc
    _ =18 iplus sget02 iplus            # i cc &xs[i]
    sset01 goto                         # &xs[i] },

  "[]" => bin q{                        # i self cc
    sget01 =18 iplus sget03 iplus       # i self cc &xs[i]
    m8get sset02 sset00 goto            # x },

  "[]=" => bin q{                       # x i self cc
    sget02 sget02 .&[]                  # x i self cc &xs[i]
    dup m8get swap                      # x i self cc x0 &xs[i]
    sget05 swap m8set                   # x i self cc x0
    sset03 sset01 drop goto             # x0 };


use phi::class i64_direct_array =>
  array_protocol,
  array_value_protocol,

  direct_array_class->methods,

  "[]" => bin q{                        # i self cc
    sget02 sget02 .&[] m64get           # i self cc x
    sset02 sset00 goto                  # x },

  "[]=" => bin q{                       # x i self cc
    sget02 sget02 .&[]                  # x i self cc &xs[i]
    dup m64get swap                     # x i self cc x0 &xs[i]
    sget05 swap m64set                  # x i self cc x0
    sset03 sset01 drop goto             # x0 };


use phi::class bit_direct_array =>
  array_protocol,
  array_value_protocol,
  bitset_protocol,

  direct_array_class->methods,

  "contains?" => bin q{sget02 sget02 .[] sset02 sset00 goto},
  add         => bin q{                 # i self cc
    =1 sget03 sget03 .[]=               # i self cc x0
    sset02 sset00 goto                  # x0 },

  "[]" => bin q{                        # i self cc
    sget02 sget02 .&[] m8get            # i self cc x
    =1 sget04 ishl iand =1 =0 if        # i self cc x?
    sset02 sset00 goto                  # x? },

  "[]=" => bin q{                       # x i self cc
    sget02 sget02 .&[]                  # x i self cc &x0
    =1 sget04 ishl                      # x i self cc &x0 bit
    sget02 m8get sget02 iand =1 =0 if _ # x i self cc &x0 b0? bit
    dup iinv sget03 m8get iand _        # x i self cc &x0 b0? x0&~bit bit
    sget07 _ =0 if ior _                # x i self cc &x0 x0' b0?
    sset05 _ m8set                      # b0? i self cc
    sset01 drop goto                    # b0? };


=head3 Indirect arrays
Broadly speaking, we have several categories of functionality here that direct
arrays don't offer:

1. Element storage management
2. Associative mapping
3. Non-forked checkpointing
4. Set semantics
5. Extension, e.g. C<+=>

(3) deserves some explanation. The contract you get from indirect arrays is a
little strange: you can use them in an immutable context as long as (a) you
don't need to maintain multiple modification paths, (b) you reset their length
before using them, and (c) all of your edits fall after the length you're
resetting to.

Basically, this amounts to "I know I've added stuff, but pretend you have only
five elements like you did before." While this isn't anything close to real
immutability, it gives us almost the same advantages from a parsing point of
view.
=cut

use phi::protocol resizable_array =>
  qw/ ensure_capacity
      capacity
      rewind_to
      += /;

use phi::protocol resizable_value_array =>
  qw/ << /;

use phi::protocol associative_array =>
  qw/ ensure_values
      vdata
      vesize_bits
      vsize
      v&[] /;

use phi::protocol associative_value_array =>
  qw/ contains?
      indexof
      {}
      {}= /;                            # NB: {}= returns self, not prior


use phi::fn ilog => bin q{              # x cc
  [ =1 sget01 ishl                      # x cc loop l 1<<l
    sget04 _ ilt                        # x cc loop l (1<<l)<x?
    [ =1 iplus sget01 goto ]            # ->loop(x cc loop l+1)
    [ sset02 drop goto ]                # l
    if goto ]                           # x cc loop
  =0 sget01 goto                        # ->loop(x cc loop l=0) };


use phi::class indirect_array =>
  array_protocol,
  resizable_array_protocol,
  associative_array_protocol,

  esize_bits  => bin q{_ =8  iplus m32get _ goto},
  vesize_bits => bin q{_ =12 iplus m32get _ goto},
  n           => bin q{_ =16 iplus m32get _ goto},
  capacity    => bin q{_ =20 iplus m32get _ goto},
  data        => bin q{_ =24 iplus m64get _ goto},
  vdata       => bin q{_ =32 iplus m64get _ goto},

  rewind_to => bin q{                   # n self cc
    sget02 sget02 =16 iplus m32set      # n self cc
    sset01 _ goto                       # self },

  size => bin q{                        # self cc
    sget01 =8  iplus m32get             # self cc esize_bits
    sget02 =16 iplus m32get itimes      # self cc size_bits
    =7 iplus =3 ishr sset01 goto        # size },

  vsize => bin q{                       # self cc
    sget01 =12 iplus m32get             # self cc vesize_bits
    sget02 =16 iplus m32get itimes      # self cc vsize_bits
    =7 iplus =3 ishr sset01 goto        # vsize },

  "&[]" => bin q{                       # i self cc
    sget01 =24 iplus m64get             # i self cc &ks[0]
    sget02 =8  iplus m32get             # i self cc &ks[0] esize_bits
    sget04 itimes =3 ishr iplus         # i self cc &ks[i]
    sset02 sset00 goto                  # &ks[i] },

  "v&[]" => bin q{                      # i self cc
    sget01 =32 iplus m64get             # i self cc &vs[0]
    sget02 =12 iplus m32get             # i self cc &vs[0] vesize_bits
    sget04 itimes =3 ishr iplus         # i self cc &vs[i]
    sset02 sset00 goto                  # &vs[i] },

  "+=" => bin q{                        # xs self cc
    sget01 .esize_bits  =7 iand         # xs self cc kbitlevel?
    sget02 .vesize_bits =7 iand ior     # xs self cc bitlevel?
    [ "can't append to a bit-level array; things will break" i.die ]
    [ goto ]
    if call                             # xs self cc

    sget02 .n sget02 .n iplus dup       # xs self cc n' n'
    sget03 .ensure_capacity drop        # xs self cc n'
    sget03 .data                        # xs self cc n' &xs[0]
    sget03 dup .n _ .&[]                # xs self cc n' &xs[0] &self[n]
    sget05 .size memcpy                 # xs self cc n'

    # Does either operand have a value array? If so, append that as well.
    sget03 .vdata sget03 .vdata ior     # xs self cc n' vs?
    [ sget03 .ensure_values drop        # xs self cc n' cc'
      sget04 .vdata                     # xs self cc n' cc' &xs.v[0]
      sget04 dup .n _ .v&[]             # xs self cc n' cc' &xs.v[0] &self.v[n]
      sget06 .vsize memcpy              # xs self cc n' cc'
      goto ]                            # xs self cc n'
    [ goto ]                            # xs self cc n'
    if call                             # xs self cc n'

    sget02 =16 iplus m32set             # xs self cc [self.n=n']
    sset01 _ goto                       # self },

  ensure_values => bin q{               # self cc
    sget01 =32 m64get                   # self cc vs?
    [ goto ]                            # self
    [ sget01 .capacity                  # self cc c
      sget02 .vesize_bits itimes        # self cc vsize_bits
      =7 iplus =3 ishr                  # self cc vsize_bytes
      i.heap_allocate                   # self cc vs'
      sget02 =32 iplus m64set           # self cc [self.vs=vs']
      goto ]                            # self
    if goto                             # self },

  ensure_capacity => bin q{             # c' self cc
    sget01 =20 iplus m32get             # c' self cc c
    sget03 _ ilt inot                   # c' self cc c>=c'?

    [ sset01 _ goto ]                   # self
    [                                   # c' self cc
      sget02 ilog =1_ ishl              # c' self cc ac

      # Do we have a value array? If so, resize both at once. We need to make
      # sure that no bytes cross between the key and value arrays, so we add
      # byte quantities rather than bit quantities.
      sget02 .vdata                     # c' self cc ac vs?

      [ sget02 .esize_bits sget01 itimes# c' self cc ac kbits
        =7 iplus =3 ishr dup            # c' self cc ac kbytes kbytes
        sget04 .vesize_bits sget03 itimes
        =7 iplus =3 ishr iplus          # c' self cc ac kbytes kvbytes
        sget02 itimes i.heap_allocate   # c' self cc ac ks' kbs
        sget01 iplus                    # c' self cc ac ks' vs'

        sget04 .data sget02             # c' self cc ac ks' vs' ks ks'
        sget06 .size memcpy             # c' self cc ac ks' vs'
        sget04 .vdata sget01            # c' self cc ac ks' vs' vs vs'
        sget06 .vsize memcpy            # c' self cc ac ks' vs'
        sget04 =32 iplus m64set         # c' self cc ac ks' [self.vs=vs']
        sget03 =24 iplus m64set         # c' self cc ac [self.ks=ks']
        sget03 =20 iplus m32set         # c' self cc [self.cap=ac]
        sset01 _ goto ]                 # self

      [                                 # c' self cc ac
        sget02 .esize_bits sget01 itimes# c' self cc ac kbits
        =7 iplus =3 ishr                # c' self cc ac kbytes
        i.heap_allocate                 # c' self cc ac ks'

        sget03 .data sget01             # c' self cc ac ks' ks ks'
        sget05 .size memcpy             # c' self cc ac ks'
        sget03 =24 iplus m64set         # c' self cc ac [self.ks=ks']
        sget02 =20 iplus m32set         # c' self cc [self.cap=ac]
        sset01 _ goto ]                 # self

      if goto ]                         # self
    if goto                             # self };


use phi::class i64_indirect_array =>
  array_protocol,
  array_value_protocol,
  resizable_array_protocol,
  resizable_value_array_protocol,
  associative_array_protocol,
  associative_value_array_protocol,

  indirect_array_class->methods,
  i64_direct_array_class->methods_only(qw/ [] []= /),

  indexof => bin q{                     # x self cc
    sget01 .data dup sget03 .size iplus # x self cc begin end
    [ sget01 sget03 ilt                 # x self cc &xi end loop &x<end?
      [ sget02 m64get sget06 ieq        # x self cc &xi end loop xi==x?
        [ drop drop                     # x self cc &xi
          sget02 .data ineg iplus       # x self cc byte_offset
          =3 ishr sset02 sset00 goto ]  # index
        [ sget02 =8 iplus sset02        # x self cc &xi' end loop
          dup goto ]                    # ->loop
        if goto ]
      [ drop drop drop sset00           # x cc
        =1 ineg sset01 goto ]           # -1
      if goto ]
    dup goto                            # ->loop(x self cc begin end) },

  "contains?" => bin q{                 # x self cc
    sget02 sget02 .indexof              # x self cc i
    =1 ineg ieq inot                    # x self cc contains?
    sset02 sset00 goto                  # contains? },

  "{}" => bin q{                        # k self cc
    sget02 sget02 .indexof              # k self cc i
    dup =1 ineg ieq                     # k self cc i missing?
    sget03 .vdata inot ior              # k self cc i missing||!vals?
    [ "{} lookup on unassociated element" i.die ]
    [ sget02 .v&[] m64get               # k self cc v
      sset02 sset00 goto ]              # v
    if goto                             # v },

  "{}=" => bin q{                       # v k self cc
    sget02 sget02 .ensure_values
                  .indexof              # v k self cc i
    dup =1 ineg ieq                     # v k self cc i missing?
    [ drop                              # v k self cc
      sget01 .n sget03 sget03 .<< drop  # v k self cc i [self<<k]
      sget04 _                          # v k self cc v i
      sget03 .v&[] m64set               # v k self cc
      sset01 sset01 goto ]              # self
    [ sget04 _ sget03 .v&[] m64set      # v k self cc
      sset01 sset01 goto ]              # self
    if goto                             # self },

  "<<" => bin q{                        # x self cc
    sget01 .n dup =1 iplus dup          # x self cc n n' n'
    sget04 .ensure_capacity             # x self cc n n' self
           =16 iplus m32set             # x self cc n [self.n=n']
    sget03 _ sget03 .[]= drop           # x self cc
    sset01 _ goto                       # self };


=head3 Constructors
We have a few short-name constructors to build different variants of these
things:

  n i64d   = direct i64[n], uninitialized
  n i8d    = direct i8[n], uninitialized
  n bitset = direct i1[n] initialized to 0
  i64i     = indirect i64[8]
  i8i      = indirect i8[32]

=cut

use phi::fn i64d => bin q{                  # n cc
  sget01 =3 ishl =18 iplus i.heap_allocate  # n cc xs
  $i64_direct_array_class sget01 m64set     # n cc xs [.class=]
  =64    sget01 =8  iplus m32set            # n cc xs [.esize_bits=]
  sget02 sget01 =12 iplus m32set            # n cc xs [.n=]
  =18    sget01 =16 iplus m16set            # n cc xs [.here_marker=]
  sset01 goto                               # xs };

use phi::fn i8d => bin q{                   # n cc
  sget01 =18 iplus i.heap_allocate          # n cc xs
  $i8_direct_array_class sget01 m64set      # n cc xs [.class=]
  =8     sget01 =8  iplus m32set            # n cc xs [.esize_bits=]
  sget02 sget01 =12 iplus m32set            # n cc xs [.n=]
  =18    sget01 =16 iplus m16set            # n cc xs [.here_marker=]
  sset01 goto                               # xs };

use phi::fn i1d => bin q{                   # n cc
  sget01 =7 iplus =3 ishr =18 iplus
  i.heap_allocate                           # n cc xs
  $bit_direct_array_class sget01 m64set     # n cc xs [.class=]
  =8     sget01 =8  iplus m32set            # n cc xs [.esize_bits=]
  sget02 sget01 =12 iplus m32set            # n cc xs [.n=]
  =18    sget01 =16 iplus m16set            # n cc xs [.here_marker=]
  sset01 goto                               # xs };

use phi::fn i64i => bin q{                  # cc
  =104 i.heap_allocate                      # cc xs
  $i64_direct_array_class sget01 m64set     # cc xs [.class=]
  =64 sget01 =8  iplus m32set               # cc xs [.ksize_bits=]
  =64 sget01 =12 iplus m32set               # cc xs [.vsize_bits=]
  =0  sget01 =16 iplus m32set               # cc xs [.n=]
  =8  sget01 =20 iplus m32set               # cc xs [.capacity=]
  dup =40 iplus sget01 =24 iplus m64set     # [.ks=]
  =0  sget01 =32 iplus m64set               # [.vs=0]
  _ goto                                    # xs };

use phi::fn i8i => bin q{                   # cc
  =104 i.heap_allocate                      # cc xs
  $i8_direct_array_class sget01 m64set      # cc xs [.class=]
  =8  sget01 =8  iplus m32set               # cc xs [.ksize_bits=]
  =8  sget01 =12 iplus m32set               # cc xs [.vsize_bits=]
  =0  sget01 =16 iplus m32set               # cc xs [.n=]
  =64 sget01 =20 iplus m32set               # cc xs [.capacity=]
  dup =40 iplus sget01 =24 iplus m64set     # [.ks=]
  =0  sget01 =32 iplus m64set               # [.vs=0]
  _ goto                                    # xs };


=head2 Unit tests
Broken out into several functions because this stuff is nontrivially involved.
=cut

use phi::testfn i1d => bin q{               #
  };

use phi::testfn i8d => bin q{               #
  };

use phi::testfn i64d => bin q{              #
  };

use phi::testfn i8i => bin q{               #
  };

use phi::testfn i64i => bin q{              #
  };


1;
