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


=head1 Frame class generation
Frames are simple: storage-wise, we just need to know how many overall things
they store, and for GC purposes we need to know the types of those things. For
example, here's a function:

  (fn (int x ptr y)
    (let (int z (g64 (iadd x y)))
      (return z)))

Here's what the frame class will look like:

  struct
  {
    hereptr class;
    hereptr calling_continuation;
    int     x;
    ptr     y;
    int     tmp0;                       # (iadd x y)
    int     tmp1;                       # (g64 tmp0)
    int     z;
  };

I may be able to eliminate C<tmp1> here, not sure yet.
=cut

package phi::frame
{
  sub new
  {
    bless { size  => 3,
            slots => { class_fn     => 0,
                       parent_frame => 1,
                       cc           => 2 } }, shift;
  }

  sub get
  {
    my ($self, $asm, $var) = @_;
    die "can't frame-get undefined variable $var"
      unless exists $$self{slots}{$var};
    $asm->fi->Sb($$self{slots}{$var} * 8)->g64;
  }

  sub set
  {
    my ($self, $asm, $var) = @_;
    $$self{slots}{$var} = $$self{size}++ unless exists $$self{slots}{$var};
    $asm->fi->Sb($$self{slots}{$var} * 8)->s64;
  }

  sub enter
  {
    my ($self, $asm) = @_;
    $asm->fi->Sl(-$$self{size} >> 3)    # f'
        ->l(0xabcd)->sget->C(1)->s64    # [.class_fn=]  FIXME
        ->fi->Sl(0)->fi->Sl(-($$self{size} >> 3) + 8)
            ->s64                       # [.parent_frame=]
        ->sf                            # [f=f']
        ->fi->Sl(16)->s64;              # [.cc=]
    # TODO: initialize memory to zeros
  }

  sub exit
  {
    my ($self, $asm) = @_;
    $asm->fi->Sl(16)->g64
        ->fi->Sl(8)->g64->sf
        ->go;
  }
}


1;
