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


=head1 Function objects
Fairly straightforward; we just need to write a header that accurately stores
the code length. Here's the struct:

  struct fn
  {
    hereptr    class;
    int        size;
    here_marker;
    byte[size] code;
  };

Because C<phi::asm> instances build code incrementally, we won't know the size
until the end. We rely on users to call C<endfn> to patch the size in.
=cut

# FIXME
heap_label fn_class_fn_hereptr => "ABCDEFGH";

sub fn { phi::asm->new(@_)->patch(fn_class_fn_hereptr => 8)->Ql(0)->here }

use constant fn_code_offset => length fn->{data};

sub phi::asm::endfn
{
  my $self = shift;
  substr($$self{data}, 8, 8) = pack Q => length $$self{data};
  $self;
}


1;
