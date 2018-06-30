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
Structs tie memory to data. Specifically, they govern things like field offsets
and object sizing.


=head3 Reverse-consed structs
Here's the idea. Structs are built like linked lists, but in reverse. When you
cons a new field onto the head of the list, you get a new struct element placed
rightwards in memory. Cells are aware of their offset/size as they are consed
up; this saves computation later on.

This model also solves the problem of prior-reference for sizing. For example,
now we can define an inline sized array:

  struct sized_array
  {
    int16    n;
    int64[n] xs;
  }

Here's the equivalent consed structure:

  var_array
  {
    name    = "xs",
    n_field = "n",
    type    = int64,
    offset  = 2,
    tail    = int_field
    {
      size   = 2,
      offset = 0,
      name   = "n",
      tail   = nil_struct_link {}
    }
  }

Each element provides data about its offset, size, and optionally functions to
compute those things. Links also provide getter/setter functions whose
signatures are identical to C<mNget> and C<mNset>:

  getter_fn : (    &struct -> val)
  setter_fn : (val &struct ->)


=head4 Fixed struct links
Here's the struct layout:

  struct fixed_struct_link
  {
    hereptr              vtable;
    baseptr<struct_link> tail;
    baseptr<string>      name;
    cell                 offset;        # -1 if computed
    cell                 size;
  }

=cut


use constant nil_struct_link_class => phi::class->new('nil_struct_link',
  maybe_nil_protocol,
  struct_link_protocol)

  ->def(
    name         => bin q{"can't call name on nil link" i.die},
    getter_fn    => bin q{"can't call getter_fn on nil link" i.die},
    setter_fn    => bin q{"can't call setter_fn on nil link" i.die},

    "nil?"       => bin q{const1 sset01 goto},

    size         => bin q{const0 sset01 goto},
    left_offset  => bin q{const0 sset01 goto},
    right_offset => bin q{const0 sset01 goto},

    size_fn         => bin q{"can't call size_fn on nil link" i.die},
    left_offset_fn  => bin q{"can't call left_offset_fn on nil link" i.die},
    right_offset_fn => bin q{"can't call right_offset_fn on nil link" i.die});


use constant nil_struct_link_instance => phi::allocation
  ->constant(pack Q => nil_struct_link_class->vtable >> heap)
  ->named('nil_struct_link_instance') >> heap;


use constant fixed_struct_link_class => phi::class->new('fixed_struct_link',
  list_protocol,
  cons_protocol,
  maybe_nil_protocol,
  struct_link_protocol)

  ->def(
    tail         => bin q{swap const8  iplus m64get swap goto},
    name         => bin q{swap const16 iplus m64get swap goto},
    left_offset  => bin q{swap const24 iplus m64get swap goto},
    size         => bin q{swap const32 iplus m64get swap goto},

    getter_fn => bin q{                 # self cc
      # Return a function whose signature is structptr -> fieldptr
      asm                               # self cc asm[]
        .sget const1 swap .l8           # self cc asm[sget01]
        sget02 .left_offset_fn          # self cc asm[sget01] lofffn
          .here swap .hereptr           # self cc asm[sget01 lit lofffn]
        .call                           # self cc asm[...call]
        .sset const1 swap .l8           # self cc asm[...call sset01]
        .goto                           # self cc asm[...sset01 goto]
      .compile                          # self cc fn[...sset01 goto]
      sset01 goto                       # fn },

    setter_fn => bin q{                 # self cc
      # Return a function that does a memcpy to set the value. The function
      # signature is (&v &struct ->), so its fully general implementation looks
      # like this:
      #
      #   dup <offset-fn> call          # v struct off
      #   sget01 iplus                  # v struct &member
      #   swap <size-fn> call           # v &member size
      #   memcpy                        #
      #
      # This is a fixed-size element, so we can replace the size-fn invocation
      # with a constant.

      # TODO: redo this API to have general-purpose objects and different ctor
      # templates to set up the fields in various ways, e.g. for fixed fields.
      # No sense in duplicating all this logic across cases.

      asm
        .dup
        sget02 .left_offset_fn          # self cc asm[...] offsetfn
          .here swap .hereptr .call     # self cc asm[dup offsetfn call]

       },

    right_offset => bin q{              # self cc
      # Is our offset computed? If so, return -1.
      sget01 .left_offset dup           # self cc loff loff
      const1 ineg ieq                   # self cc loff computed?
      [ drop const1 ineg sset01 goto ]  # -1
      [ sget02 .size iplus              # self cc roff
        sset01 goto ]                   # roff
      if goto                           # off|-1 },

    "nil?" => bin q{sset00 const0 swap goto},
    head   => bin q{swap .name swap goto},

    "[]"   => bin q{                    # i self cc
      sget02                            # i self cc i
      [ sget02 const1 ineg iplus        # i self cc i-1
        sset02 swap .tail swap          # i-1 tail cc
        sget01 m64get :[] goto ]        # tail.[](i-1)
      [ sset01 swap goto ]
      if goto },

    length => bin q{                    # self cc
      swap .tail .length                # cc tail.len
      const1 iplus swap goto            # tail.len+1 },

    reduce => bin q{                    # x0 f self cc
      sget01 sget04 sget04 call         # x0 f self cc x0' exit?
      [ sset03 sset01 drop goto ]       # x0
      [ sset03 swap .tail swap          # x0' f tail cc
        sget01 m64get :reduce goto ]    # tail.reduce(...)
      if goto                           # x0 },

    size_fn => bin q{                   # self cc
      # Return our size as a constant
      asm                               # self cc asm
        .lit32                          # self cc asm[lit32]
        sget02 .size                    # self cc asm[lit32] size
        bswap32 swap .l32               # self cc asm[lit32 size]
        .sset                           # self cc asm[lit32 size sset]
        const1 swap .l8                 # self cc asm[lit32 size sset01]
        .goto                           # self cc asm[lit32 size sset01 goto]
      .compile                          # self cc fn[lit32 size sset01 goto]
      sset01 goto                       # fn },

    right_offset_fn => bin q{           # self cc
      # This is pretty simple: call the left offset function and then add a
      # constant to whatever it returns. Our stack looks like this:
      #
      #   &structptr cc
      #
      # ...so we want to do this:
      #
      #   sget01                        # ptr cc ptr
      #   $left_offset_fn call          # ptr cc loff
      #   lit32 oursize iplus           # ptr cc roff
      #   sset01 goto                   # roff

      asm                               # self cc asm[]
        .sget                           # self cc asm[sget]
        const1 swap .l8                 # self cc asm[sget01]
        sget02 .left_offset_fn          # self cc asm[sget01] fn
          .here swap .hereptr           # self cc asm[sget01 'fn]
        .call                           # self cc asm[sget01 'fn call]
        .lit32                          # self cc asm[sget01 'fn call lit32]
        sget02 .size                    # self cc asm[...lit32] size
        bswap32 swap .l32               # self cc asm[...lit32 size]
        .iplus                          # self cc asm[...size iplus]
        .sset                           # self cc asm[...iplus sset]
        const1 swap .l8                 # self cc asm[...sset01]
        .goto                           # self cc asm[...goto]
      .compile                          # self cc fn[...goto]
      sset01 goto                       # fn[...] },

    left_offset_fn => bin q{            # self cc
      sget01 .tail .right_offset_fn     # self cc fn
      sset01 goto                       # fn });


1;
