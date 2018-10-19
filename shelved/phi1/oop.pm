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


=head2 Class generator
Really this just amounts to a function that consumes a method map and emits a
dispatch function for it. The dispatch function is a template that closes over a
method table:

  lit64 <table> swap $mlookup_fn goto

It's useful to have C<table> be a hereptr to an C<i64d>; that way we can inspect
it. Its layout is C<hash1 fn1 hash2 fn2 ... hashN fnN 0>.
=cut

use phi::fn dispatch_table => bin q{    # m cc
  sget01 .n =1 ishl =1 ior dup i64d _   # m cc t slots
  =0_ =1 ineg iplus _ sget02            # m cc t 0 slots-1 t
  .[]= drop                             # m cc t

  dup .data                             # m cc t td
  sget03 .n =0                          # m cc t td n i=0
  [ sget02 sget02 ilt                   # m cc t td n i loop i<n?
    [ sget01 sget07 .[]  sget04 m64set          # [.hash=]
      sget01 sget07 .v[] sget04 =8 iplus m64set # [.fn=]
      sget03 =16 iplus sset03           # td+=16
      _ =1 iplus _                      # i+=1
      dup goto ]                        # ->loop
    [ drop drop drop drop sset01 goto ] # t
    if goto ]
  dup goto                              # ->loop };

use phi::fn dispatch_fn => bin q{       # m cc
  _ dispatch_table .data                # cc td
  asm
    .hereptr                            # cc asm[td]
    .swap $mlookup_fn _ .hereptr
    .goto                               # cc asm
  .compile .data _ goto                 # fn };


=head3 Unit tests
Most of the work is happening for us in C<method_hash>, but let's make sure the
above code allocates tables correctly.
=cut

use phi::protocol oop_test => qw/ inc dec val /;

use phi::testfn oop => bin q{           #
  i64i                                  # m
    [                                   # self cc
      sget01 =8 iplus dup m64get        # self cc &n n
      =1 iplus _ m64set goto ] _        # fn m
    "inc" mh _ .{}=                     # m

    [                                   # self cc
      sget01 =8 iplus dup m64get        # self cc &n n
      =1 ineg iplus _ m64set goto ] _   # fn m
    "dec" mh _ .{}=                     # m

    [ _ =8 iplus m64get _ goto ] _
    "val" mh _ .{}=                     # m

  =3 _                                  # val m
  dispatch_fn                           # val fn
  get_stackptr                          # val fn &obj

  dup      .val =3 ieq "val3"  i.assert
  dup .inc .val =4 ieq "inc4"  i.assert
  dup .inc .val =5 ieq "inc5"  i.assert
  dup .dec .val =4 ieq "dec4"  i.assert
  dup .dec .val =3 ieq "dec3"  i.assert
  dup .dec .val =2 ieq "dec2"  i.assert
  sget02        =2 ieq "sget2" i.assert

  drop drop drop                        # };


1;
