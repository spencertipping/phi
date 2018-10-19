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
    int32_t  ksize_bits;                # offset=8
    int32_t  vsize_bits;                # offset=12
    int32_t  n;                         # offset=16
    int32_t  capacity;                  # offset=20
    K       *ks;                        # offset=24
    V       *vs;                        # offset=32
  };

These arrays can be used as associative maps because they contain two element
stores, although C<vs> remains null until you use associative features.
=cut

use phi::protocol array =>
  qw/ n
      reduce
      &[]
      esize_bits
      size
      data /;

use phi::protocol array_value =>
  qw/ []
      []= /;                            # NB: []= should return prior element

use phi::protocol bitset =>
  qw/ add
      <<
      ~
      contains? /;


use phi::fn reduce_array => bin q{      # x0 f xs cc
  sget01 .n =0                          # x0 f xs cc n i
  [ sget02 sget02 ilt                   # x0 f xs cc n i loop i<n?
    [ sget01 sget05 .[]                 # x0 f xs cc n i loop xs[i]
      sget07 sget07 call                # x0 f xs cc n i loop x1 exit?
      [ sset06 drop drop drop           # x1 f xs cc
        sset01 drop goto ]              # x1
      [ sset06 _ =1 iplus _ dup goto ]  # ->loop(x1 f xs cc n i+1 loop)
      if goto ]                         # ...
    [ drop drop drop sset01 drop goto ] # x0
    if goto ]                           # x0 f xs cc n i loop
  dup goto                              # ->loop };

use phi::fn arrays_eq => bin q{         # ys xs cc
  sget02 sget02 ieq
  [ drop sset00 =1 sset01 goto ]        # 1
  [ goto ] if call                      # ys xs cc

  sget02 .n sget02 .n ixor
  [ drop sset00 =0 sset01 goto ]        # 0
  [ goto ] if call                      # ys xs cc

  sget01 .n =0                          # ys xs cc n i=0
  [ sget02 sget02 ilt                   # ys xs cc n i loop i<n?
    [ sget01 sget06 .[]                 # ys xs cc n i loop ys[i]
      sget02 sget06 .[] ieq             # ys xs cc n i loop xs[i]==ys[i]?
      [ _ =1 iplus _ dup goto ]         # ->loop(i+1)
      [ drop drop drop sset00 =0 sset01 goto ]
      if goto ]
    [ drop drop drop sset00 =1 sset01 goto ]
    if goto ]
  dup goto                              # ->loop };

use phi::fn rev => bin q{               # xs cc
  sget01 .n =1 ineg iplus =0_           # xs cc left=0 right=n-1
  [ sget01 sget03 ilt                   # xs cc l r loop l<r?
    [ sget02 sget05 .[]                 # xs cc l r loop xs[l]
      sget02 sget06 .[]=                # xs cc l r loop xs[r]
      sget03 sget06 .[]= drop           # xs cc l r loop
      _ =1 ineg iplus _                 # xs cc l r-1 loop
      sget02 =1 iplus sset02            # xs cc l+1 r-1 loop
      dup goto ]                        # ->loop
    [ drop drop drop goto ]             # xs
    if goto ]                           # xs cc l r loop
  dup goto                              # ->loop };


use phi::class direct_array =>
  clone_protocol,
  eq_protocol,
  array_protocol,
  joinable_protocol,

  clone => bin q{                       # self cc
    sget01.size =18 iplus               # self cc size
    dup i.heap_allocate                 # self cc size new
    sget03 sget01 sget03 memcpy         # self cc size new [new=self]
    sset02 drop goto                    # new },

  "+" => bin q{                         # rhs self cc
    sget02.esize_bits sget02.esize_bits ieq
    [ goto ]
    [ "can't join two arrays whose element sizes differ" i.die ]
    if call

    sget02.esize_bits sget02.esize_bits ior =7 iand
    [ "can't join arrays whose elements cross byte boundaries" i.die ]
    [ goto ]
    if call

    sget02.size sget02.size iplus       # rhs self cc size
    =18 iplus i.heap_allocate           # rhs self cc new
    sget02 sget01                       # rhs self cc new self new
      sget01.size =18 iplus memcpy      # rhs self cc new [new=self]
    sget03.data sget01.data             # rhs self cc new &rhs[0] &new[0]
      sget04.size iplus                 # rhs self cc new &rhs[0] &new[n]
      sget05.size memcpy                # rhs self cc new [new[n..]=rhs]

    # Now sum the element counts of the two arrays.
    sget03.n sget03.n iplus             # rhs self cc new n'
    sget01 =12 iplus m32set             # rhs self cc new [new.n=]
    sset02 sset00 goto                  # new },

  esize_bits => bin q{_ =8  iplus m32get _ goto},
  n          => bin q{_ =12 iplus m32get _ goto},
  data       => bin q{_ =18 iplus        _ goto},

  reduce => bin q{$reduce_array_fn goto},
  "=="   => bin q{$arrays_eq_fn goto},

  size => bin q{                        # self cc
    sget01 =8  iplus m32get             # self cc esize_bits
    sget02 =12 iplus m32get itimes      # self cc size_bits
    =7 iplus =3 ishr sset01 goto        # size_bytes },

  # NB: for bitfields, this returns a pointer to the byte that contains the bit.
  "&[]" => bin q{                       # i self cc
    sget01 =18 iplus                    # i self cc &xs[0]
    sget02 =8  iplus m32get             # i self cc &xs[0] esize_bits
    sget04 itimes =3 ishr iplus         # i self cc &xs[i]
    sset02 sset00 goto                  # &xs[i] };


use phi::class i8_direct_array =>
  direct_array_class->protocols,
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


heap->initialize(str_dispatch_fn => pack Q => i8_direct_array_class);


use phi::class i64_direct_array =>
  direct_array_class->protocols,
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
  direct_array_class->protocols,
  array_value_protocol,
  bitset_protocol,

  direct_array_class->methods,

  "contains?" => bin q{sget02 sget02 .[] sset02 sset00 goto},
  add         => bin q{                 # i self cc
    =1 sget03 sget03 .[]=               # i self cc x0
    sset02 sset00 goto                  # x0 },

  "<<" => bin q{                        # i self cc
    sget02 sget02 .add drop             # i self cc
    sset01 _ goto                       # self },

  "~" => bin q{                         # self cc
    _ .clone _ sget01 .size             # new cc bytes
    sget02 .data _                      # new cc &new[0] bytes
    sget01 iplus _                      # new cc &new[size] &new[0]
    [ sget02 sget02 ilt                 # new cc &new[size] &new[i] loop ni<ns?
      [ sget01 m8get iinv               # new cc ns ni loop ~vi
        sget02 m8set                    # new cc ns ni loop
        _ =1 iplus _ dup goto ]         # ->loop(ni+1)
      [ drop drop drop goto ]           # new
      if goto ]                         # new cc ns n0 loop
    dup goto                            # ->loop(n0) },

  "[]" => bin q{                        # i self cc
    sget02 sget02 .&[] m8get            # i self cc x
    =1 sget04 =7 iand ishl iand =1 =0 if# i self cc x?
    sset02 sset00 goto                  # x? },

  "[]=" => bin q{                       # x i self cc
    sget02 sget02 .&[]                  # x i self cc &x0
    =1 sget04 =7 iand ishl              # x i self cc &x0 bit
    sget01 m8get sget01 iand =1 =0 if _ # x i self cc &x0 b0? bit
    dup iinv sget03 m8get iand _        # x i self cc &x0 b0? x0&~bit bit
    sget07 _ =0 if ior _                # x i self cc &x0 x0' b0?
    sset05 _ m8set                      # b0? i self cc
    sset01 drop goto                    # b0? };


=head3 Constructors
We have a few short-name constructors to build different variants of these
things:

  n i64d = direct i64[n], uninitialized
  n i8d  = direct i8[n], uninitialized
  n i1d  = direct i1[n] initialized to 0

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
    i.heap_allocate                         # n cc xs
  $bit_direct_array_class sget01 m64set     # n cc xs [.class=]
  =1     sget01 =8  iplus m32set            # n cc xs [.esize_bits=]
  sget02 sget01 =12 iplus m32set            # n cc xs [.n=]
  =18    sget01 =16 iplus m16set            # n cc xs [.here_marker=]
  =0 sget01 =18 iplus                       # n cc xs 0 xs.data
    sget04 =7 iplus =3 ishr memset          # n cc xs
  sset01 goto                               # xs };


=head3 C<str> constructor
Allocate strings as C<i8_direct_array>s.
=cut

{
  no warnings 'redefine';
  sub str($)
  {
    once "string const \"" . ($_[0] =~ s/[[:cntrl:]]/./gr)
                           . "\""
                           . ++($phi::str_index //= 0),
         pack "QLLSa*" => i8_direct_array_class,
                          1,
                          length $_[0],
                          18,
                          $_[0];
  }
}


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
      rewind
      to_direct
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
      v[]
      {}
      {}= /;                            # NB: {}= returns self, not prior


use phi::fn ilog => bin q{              # x cc
  [ =1 sget01 ishl                      # x cc loop l 1<<l
    sget04 _ ilt                        # x cc loop l (1<<l)<x?
    [ =1 iplus sget01 goto ]            # ->loop(x cc loop l+1)
    [ sset02 drop goto ]                # l
    if goto ]                           # x cc loop
  =0 sget01 goto                        # ->loop(x cc loop l=0) };


use phi::fn indirect_to_direct => bin q{# xs cc
  sget01 .size =18 iplus i.heap_allocate    # xs cc ys
  $direct_array_class sget01 m64set         # xs cc ys [.class=]
  sget02 .esize_bits sget01 =8 iplus m32set # xs cc ys [.esize_bits=]
  sget02 .n          sget01 =12 iplus m32set# xs cc ys [.n=]
  =18                sget01 =16 iplus m16set# xs cc ys [.here_marker=]

  sget02 .data sget01 .data             # xs cc ys &xs[0] &ys[0]
    sget02 .size memcpy                 # xs cc ys [ys=xs]
  sset01 goto                           # ys };


use phi::class indirect_array =>
  clone_protocol,
  array_protocol,
  resizable_array_protocol,
  associative_array_protocol,
  joinable_protocol,

  esize_bits  => bin q{_ =8  iplus m32get _ goto},
  vesize_bits => bin q{_ =12 iplus m32get _ goto},
  n           => bin q{_ =16 iplus m32get _ goto},
  capacity    => bin q{_ =20 iplus m32get _ goto},
  data        => bin q{_ =24 iplus m64get _ goto},
  vdata       => bin q{_ =32 iplus m64get _ goto},

  reduce => bin q{$reduce_array_fn goto},

  clone => bin q{                       # self cc
    =40 i.heap_allocate                 # self cc new
    sget02 sget01 =40 memcpy            # self cc new [new=self]
    =0 sget01 =16 iplus m64set          # self cc new [new.n=new.cap=0]
    sget02 _ .+=                        # self cc new [new+=self]
    sset01 goto                         # new },

  to_direct => bin q{$indirect_to_direct_fn goto},

  rewind => bin q{                      # delta self cc
    sget01 .n sget03 ineg iplus         # delta self cc n'
    sset02 sget01 m64get :rewind_to goto# ->self.rewind_to(n' self cc) },

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
    sget01 .esize_bits                  # xs self cc kbits
    sget02 .vesize_bits ior =7 iand     # xs self cc bitlevel?
    [ "can't append to a bit-level array; things will break" i.die ]
    [ goto ]
    if call                             # xs self cc

    sget02 .n sget02 .n iplus dup       # xs self cc n' n'
    sget03 .ensure_capacity drop        # xs self cc n'
    sget03 .data                        # xs self cc n' &xs[0]
    sget03 dup .n _ .&[]                # xs self cc n' &xs[0] &self[n]
    sget05 .size memcpy                 # xs self cc n'

    # Does the LHS have a value array? If so, append that as well. If the RHS
    # has a value array and you want that, use .ensure_values on the LHS before
    # calling +=.
    sget02 .vdata                       # xs self cc n' vs?
    [ sget03 .ensure_values drop        # xs self cc n' cc'
      sget04 .vdata                     # xs self cc n' cc' &xs.v[0]
      sget04 dup .n _ .v&[]             # xs self cc n' cc' &xs.v[0] &self.v[n]
      sget06 .vsize memcpy              # xs self cc n' cc'
      goto ]                            # xs self cc n'
    [ goto ]                            # xs self cc n'
    if call                             # xs self cc n'

    sget02 =16 iplus m32set             # xs self cc [self.n=n']
    sset01 _ goto                       # self },

  "+" => bin q{                         # rhs self cc
    _ .clone _                          # rhs clone cc
    sget01 m64get :+= goto              # ->clone.+= },

  ensure_values => bin q{               # self cc
    sget01 =32 iplus m64get             # self cc vs?
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
  indirect_array_class->protocols,
  array_value_protocol,
  resizable_value_array_protocol,
  associative_value_array_protocol,

  indirect_array_class->methods_except(qw/ to_direct /),
  i64_direct_array_class->methods_only(qw/ [] []= /),

  to_direct => bin q{                   # self cc
    _ indirect_to_direct                # cc new
    $i64_direct_array_class sget01 m64set
    _ goto                              # new },

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

  "v[]" => bin q{                       # i self cc
    sget02 sget02 .v&[] m64get          # i self cc v
    sset02 sset00 goto                  # v },

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


use phi::class i8_indirect_array =>
  indirect_array_class->protocols,
  array_value_protocol,
  resizable_value_array_protocol,
  associative_array_protocol,

  indirect_array_class->methods_except(
    qw/ to_direct vesize_bits ensure_values vsize v&[] vdata /),

  vesize_bits   => bin q{=0 sset01 goto},
  vdata         => bin q{=0 sset01 goto},
  vsize         => bin q{=0 sset01 goto},
  "v&[]"        => bin q{"i8 indirects don't support values" i.die},
  ensure_values => bin q{"i8 indirects don't support values" i.die},

  to_direct => bin q{                   # self cc
    _ indirect_to_direct                # cc new
    $i8_direct_array_class sget01 m64set
    _ goto                              # new },

  "[]" => bin q{                        # i self cc
    sget02 sget02 .&[] m8get            # i self cc x
    sset02 sset00 goto                  # x },

  "[]=" => bin q{                       # x i self cc
    sget02 sget02 .&[] dup m8get _      # x i self cc x0 &x
    sget05 _ m8set                      # x i self cc x0
    sset03 sset01 drop goto             # x0 },

  "<<" => bin q{                        # x self cc
    sget01 .n dup =1 iplus dup          # x self cc n n' n'
    sget04 .ensure_capacity             # x self cc n n' self
           =16 iplus m32set             # x self cc n [self.n=n']
    sget03 _ sget03 .[]= drop           # x self cc
    sset01 _ goto                       # self };


=head3 Constructors
Indirect arrays start empty, so no size is specified for constructors. Capacity
is initially set to 64 bytes of stuff, which is slightly more than the absolute
overhead of the array structure itself. This balances small-array expansion
overhead with initial memory commitment.

  i64i = indirect i64[8]
  i8i  = indirect i8[64]

=cut

use phi::fn i64i => bin q{                  # cc
  =104 i.heap_allocate                      # cc xs
  $i64_indirect_array_class sget01 m64set   # cc xs [.class=]
  =64 sget01 =8  iplus m32set               # cc xs [.ksize_bits=]
  =64 sget01 =12 iplus m32set               # cc xs [.vsize_bits=]
  =0  sget01 =16 iplus m32set               # cc xs [.n=]
  =8  sget01 =20 iplus m32set               # cc xs [.capacity=]
  dup =40 iplus sget01 =24 iplus m64set     # [.ks=]
  =0  sget01 =32 iplus m64set               # [.vs=0]
  _ goto                                    # xs };

use phi::fn i8i => bin q{                   # cc
  =104 i.heap_allocate                      # cc xs
  $i8_indirect_array_class sget01 m64set    # cc xs [.class=]
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
  =64 i1d                                   # bs
  =18 iplus unhere
  dup .n    =64 ieq "i1d.n"    i.assert
  dup .size =8  ieq "i1d size" i.assert     # bs
  dup =0_  .[] =0 ieq "i1d[0]"  i.assert
  dup =1_  .[] =0 ieq "i1d[1]"  i.assert
  dup =2_  .[] =0 ieq "i1d[2]"  i.assert
  dup =3_  .[] =0 ieq "i1d[3]"  i.assert
  dup =4_  .[] =0 ieq "i1d[4]"  i.assert
  dup =5_  .[] =0 ieq "i1d[5]"  i.assert
  dup =6_  .[] =0 ieq "i1d[6]"  i.assert
  dup =7_  .[] =0 ieq "i1d[7]"  i.assert
  dup =63_ .[] =0 ieq "i1d[63]" i.assert

  =0 sget01 .add =0 ieq "i1d[0].add" i.assert   # bs
  dup =0_ .[] =1 ieq "i1d[0]'" i.assert
  dup =1_ .[] =0 ieq "i1d[1]'" i.assert
  =0 sget01 .add =1 ieq "i1d[0]'.add" i.assert
  dup =0_ .[] =1 ieq "i1d[0]''" i.assert
  dup =1_ .[] =0 ieq "i1d[1]''" i.assert

  drop };

use phi::testfn i8d => bin q{               #
  "foo" .clone                              # "foo"
  =18 iplus unhere
  dup     .n    =3 ieq "foo.n"    i.assert
  dup     .size =3 ieq "foo.size" i.assert
  dup =0_ .[] =102 ieq "foo[0]"   i.assert
  dup =1_ .[] =111 ieq "foo[1]"   i.assert
  dup =2_ .[] =111 ieq "foo[2]"   i.assert

  dup =97_ =0_ .[]= =102 ieq "foo[0]="  i.assert
  dup =0_ .[]        =97 ieq "foo[0]'"  i.assert
  dup =1_ .[]       =111 ieq "foo[1]'"  i.assert
  dup =2_ .[]       =111 ieq "foo[2]'"  i.assert
  dup =98_ =1_ .[]= =111 ieq "foo[1]="  i.assert
  dup =0_ .[]        =97 ieq "foo[0]''" i.assert
  dup =1_ .[]        =98 ieq "foo[1]''" i.assert
  dup =2_ .[]       =111 ieq "foo[2]''" i.assert

  drop

  "foo" "foo" .==      "foo==foo" i.assert
  "foo" "fo"  .== inot "fo!=foo"  i.assert
  "foo" "bar" .== inot "bar!=foo" i.assert

  "foo" "bar"_ .+ "foobar" .== "foo+bar" i.assert

  =7 i8d .size =7 ieq "i8d size" i.assert
  =7 i8d .n    =7 ieq "i8d n"    i.assert };

use phi::testfn i64d => bin q{              #
  =5 i64d                                   # xs
  =18 iplus unhere
  dup .n    =5  ieq "i64d.n"    i.assert
  dup .size =40 ieq "i64d.size" i.assert
  dup =1_ =0_ .[]= drop
  dup =2_ =1_ .[]= drop
  dup =3_ =2_ .[]= drop
  dup =4_ =3_ .[]= drop
  dup =5_ =4_ .[]= drop

  dup =0_ .[] =1 ieq "i64d[0]" i.assert
  dup =1_ .[] =2 ieq "i64d[1]" i.assert
  dup =2_ .[] =3 ieq "i64d[2]" i.assert
  dup =3_ .[] =4 ieq "i64d[3]" i.assert
  dup =4_ .[] =5 ieq "i64d[4]" i.assert
  drop };

use phi::testfn i8i => bin q{               #
  i8i                                       # xs
  dup .n         =0 ieq "i8i.n"    i.assert
  dup .size      =0 ieq "i8i.size" i.assert
  dup .capacity =64 ieq "i8i.cap"  i.assert
  =1_ .<<                                   # xs
  dup      .n =1 ieq "i8i.n'"    i.assert
  dup   .size =1 ieq "i8i.size'" i.assert
  dup =0_ .[] =1 ieq "i8i[0]"    i.assert

  "foo"_ .+=                                # xs
  dup         .n =4 ieq "i8i.n''"    i.assert
  dup      .size =4 ieq "i8i.size''" i.assert
  dup .capacity =64 ieq "i8i.cap''"  i.assert
  dup =0_ .[] =1   ieq "i8i[0]''" i.assert
  dup =1_ .[] =102 ieq "i8i[1]''" i.assert
  dup =2_ .[] =111 ieq "i8i[2]''" i.assert
  dup =3_ .[] =111 ieq "i8i[3]''" i.assert

  dup .to_direct =18 iplus unhere
    dup      .n =4   ieq "i8d.n''"   i.assert
    dup   .size =4   ieq "i8d.cap''" i.assert
    dup =0_ .[] =1   ieq "i8d[0]''"  i.assert
    dup =1_ .[] =102 ieq "i8d[1]''"  i.assert
    dup =2_ .[] =111 ieq "i8d[2]''"  i.assert
    dup =3_ .[] =111 ieq "i8d[3]''"  i.assert
  drop

  dup .+=                                   # xs
  dup .n         =8 ieq "i8i.n8"    i.assert
  dup .size      =8 ieq "i8i.size8" i.assert
  dup .capacity =64 ieq "i8i.cap8"  i.assert

  dup .+=                                   # xs
  dup .n        =16 ieq "i8i.n16"    i.assert
  dup .size     =16 ieq "i8i.size16" i.assert
  dup .capacity =64 ieq "i8i.cap16"  i.assert

  dup .+=                                   # xs
  dup .n        =32 ieq "i8i.n32"    i.assert
  dup .size     =32 ieq "i8i.size32" i.assert
  dup .capacity =64 ieq "i8i.cap32"  i.assert

  dup .+=                                   # xs
  dup .n        =64 ieq "i8i.n64"    i.assert
  dup .size     =64 ieq "i8i.size64" i.assert
  dup .capacity =64 ieq "i8i.cap64"  i.assert

  =19_ .<<                                  # xs
  dup .n        =65  ieq "i8i.n65"     i.assert
  dup .size     =65  ieq "i8i.size65"  i.assert
  dup .capacity =128 ieq "i8i.cap128"  i.assert
  dup =0_     .[] =1 ieq "i8i[0]'''"   i.assert
  dup =64_   .[] =19 ieq "i8i[64]'''"  i.assert

  dup .to_direct =18 iplus unhere
    dup       .n =65  ieq "i8d.n'''"   i.assert
    dup    .size =65  ieq "i8d.cap'''" i.assert
    dup =0_  .[] =1   ieq "i8d[0]'''"  i.assert
    dup =1_  .[] =102 ieq "i8d[1]'''"  i.assert
    dup =2_  .[] =111 ieq "i8d[2]'''"  i.assert
    dup =3_  .[] =111 ieq "i8d[3]'''"  i.assert
    dup =64_ .[] =19  ieq "i8d[64]'''" i.assert
  drop

  dup .clone .to_direct =18 iplus unhere
    dup       .n =65  ieq "i8d.n'''"   i.assert
    dup    .size =65  ieq "i8d.cap'''" i.assert
    dup =0_  .[] =1   ieq "i8d[0]'''"  i.assert
    dup =1_  .[] =102 ieq "i8d[1]'''"  i.assert
    dup =2_  .[] =111 ieq "i8d[2]'''"  i.assert
    dup =3_  .[] =111 ieq "i8d[3]'''"  i.assert
    dup =64_ .[] =19  ieq "i8d[64]'''" i.assert
  drop

  dup .+=                                   # xs
  dup .n        lit16 0082 ieq "i8i.n130"   i.assert
  dup .capacity =256       ieq "i8i.cap256" i.assert

  drop };

use phi::testfn i64i => bin q{              #
  i64i                                      # xs
  dup .n        =0 ieq "i64i.n"   i.assert
  dup .capacity =8 ieq "i64i.cap" i.assert

  lit64 0123456789abcdef _ .<<              # xs
  dup .n        =1 ieq "i64i.n1"    i.assert
  dup .capacity =8 ieq "i64i.cap1"  i.assert
  dup =0_ .[] lit64 0123456789abcdef ieq "i64i[0]" i.assert

  =5 _ .<<                                  # xs
  dup .n        =2 ieq "i64i.n2"    i.assert
  dup .capacity =8 ieq "i64i.cap2"  i.assert
  dup =0_ .[] lit64 0123456789abcdef ieq "i64i[0]" i.assert
  dup =1_ .[]                     =5 ieq "i64i[1]" i.assert

  dup .+=                                   # xs
  dup .+=                                   # xs
  dup .n        =8 ieq "i64i.n8"    i.assert
  dup .capacity =8 ieq "i64i.cap8"  i.assert
  dup =0_ .[] lit64 0123456789abcdef ieq "i64i[0]" i.assert
  dup =1_ .[]                     =5 ieq "i64i[1]" i.assert
  dup =6_ .[] lit64 0123456789abcdef ieq "i64i[6]" i.assert
  dup =7_ .[]                     =5 ieq "i64i[7]" i.assert

  =99_ .<<                                  # xs
  dup .n        =9  ieq "i64i.n9"    i.assert
  dup .capacity =16 ieq "i64i.cap9"  i.assert
  dup =0_ .[] lit64 0123456789abcdef ieq "i64i[0]" i.assert
  dup =1_ .[]                     =5 ieq "i64i[1]" i.assert
  dup =6_ .[] lit64 0123456789abcdef ieq "i64i[6]" i.assert
  dup =7_ .[]                     =5 ieq "i64i[7]" i.assert
  dup =8_ .[]                    =99 ieq "i64i[8]" i.assert

  .to_direct =18 iplus unhere
  dup .n    =9  ieq "i64d.n9"     i.assert
  dup .size =72 ieq "i64d.size72" i.assert
  dup =0_ .[] lit64 0123456789abcdef ieq "i64d[0]" i.assert
  dup =1_ .[]                     =5 ieq "i64d[1]" i.assert
  dup =6_ .[] lit64 0123456789abcdef ieq "i64d[6]" i.assert
  dup =7_ .[]                     =5 ieq "i64d[7]" i.assert
  dup =8_ .[]                    =99 ieq "i64d[8]" i.assert

  drop

  i64i
  =1_ =2_ .{}=
  dup .n =1 ieq "i64i {}=.n 1" i.assert
  dup =2_ .{} =1_ ieq "i64i {} 1" i.assert
  drop };


=head2 C<mhash> compatibility
We need to be able to match phi0's method hashing strategy at runtime so we can
generate code that interoperates. This should be covered by dedicated tests.
=cut

use phi::fn murmur2a => bin q{          # s seed cc
  sget02 .size sget02 ixor              # s seed cc h
  [                                     # s seed cc h loop &d n i
    sget01 sget01 lit8+7 iplus ilt      # s seed cc h loop &d n i i+7<n?
    [                                   # s seed cc h loop &d n i
      sget02 sget01 iplus m64get        # s seed cc h loop &d n i k
      lit64 c6a4a793 5bd1e995 itimes    # s seed cc h loop &d n i k'
      dup =47 isar ixor                 # s seed cc h loop &d n i k''
      lit64 c6a4a793 5bd1e995 itimes    # s seed cc h loop &d n i k'''

      sget05 ixor                       # s seed cc h loop &d n i h'
      lit64 c6a4a793 5bd1e995 itimes    # s seed cc h loop &d n i h''
      sset04                            # s seed cc h'' loop &d n i
      =8     iplus                      # s seed cc h'' loop &d n i+8

      sget03 goto ]                     # ->loop

    [ # Fewer than 8 bytes left: mix in a partial little-endian qword. We
      # need to build this up byte by byte; otherwise we risk running beyond
      # the string, and ultimately beyond a page boundary, which could cause a
      # segfault.
      #
      # Do we have anything left at all? If not, then we're done.

      sget01 sget01 ilt                 # s seed cc h _ &d n i i<n?
      [ =0                              # s seed cc h _ &d n i k
        [ sget02 sget02 ilt             # s seed cc h _ &d n i k i<n?
          [ sget03 sget02 iplus m8get   # s seed cc h loop' &d n i  k d[i]
            sget02 =7 iand              # s seed cc h loop' &d n i  k d[i] bi
            =3 ishl ishl ior            # s seed cc h loop' &d n i  k'
            swap =1 iplus swap          # s seed cc h loop' &d n i' k'
            sget04 goto ]               # ->loop'

          [ lit64 c6a4a793 5bd1e995 itimes  # s seed cc h _ &d n i k'
            dup =47 isar ixor               # s seed cc h _ &d n i k''
            lit64 c6a4a793 5bd1e995 itimes  # s seed cc h _ &d n i k'''
            sget05 ixor                     # s seed cc h _ &d n i h'
            lit64 c6a4a793 5bd1e995 itimes  # s seed cc h _ &d n i h''

            sset07 drop drop drop drop drop
            sset00 goto ]               # h''
          if goto ]                     # s seed cc h _ &d n i k loop'
        dup sset05 goto ]               # ->loop'

      [                                 # s seed cc h _ &d n i
        drop drop drop drop             # s seed cc h
        sset02 sset00 goto ]            # h
      if goto ]
    if goto ]                           # s seed cc h loop

  sget04 dup .data swap .size =0        # s seed cc h loop &d n i
  sget03 goto                           # ->loop };


use phi::binmacro method_hash => bin q{=0 murmur2a =1 ior};
use phi::binmacro mh          => bin q{method_hash};


sub mhash_test($)
{
  bin qq{
    lit64 >pack("Q>", method_hash "$_[0]")
    "$_[0]" method_hash ieq "$_[0]" i.assert };
}

sub all_mhash_tests()
{
  join"", map mhash_test($_), sort keys %{+defined_methods};
}


use phi::testfn mhash => bin q{
  # Important: we need the same hashed value from both perl and from phi
  # (otherwise phi won't be able to compile compatible method calls)
  >mhash_test "a"
  >mhash_test "ab"
  >mhash_test "abc"
  >mhash_test "abcd"
  >mhash_test "abcde"
  >mhash_test "abcdef"
  >mhash_test "abcdefg"
  >mhash_test "abcdefgh"
  >mhash_test "abcdefghabcdefgh"
  >mhash_test "foobarbifbazbok"
  >mhash_test "foobarbifbazbokzzz"
  >all_mhash_tests };


1;
