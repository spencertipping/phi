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


=head2 Defining tests
I want to make sure I don't leave out any unit tests, so we keep a list of all
defined unit-testing functions. They're defined using C<use phi::testfn> using a
syntax identical to C<phi::fn> except that C<goto> is automatically appended:

  use phi::testfn linked_list => bin q{ ... };

To run all unit tests:

  bin q{
    ...
    >test_runner_code
    ...
  }

=cut

use constant test_list => [];
use constant test_fns => {};

BEGIN
{
  ++$INC{"phi/testfn.pm"};
}

sub phi::testfn::import
{
  my ($self, $name, $fn) = @_;
  my $fn_addr = (phi::allocation->constant($fn . bin q{goto})
                                ->named("test $name") >> heap)->address;
  push @{+test_list}, $name;
  test_fns->{$name} = $fn_addr;
}


1;
