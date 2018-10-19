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


=head2 Profile export
Depending on the phi0 build settings, we may end up accumulating a detailed
count of method invocations by receivers. We should export that to FD 3.
=cut

use phi::fn print_prof_counter => bin q{# type name counter cc
  _ m64get                              # type name cc count
  strbuf                                # type name cc count buf
            .<<dec                      # type name cc buf
    =9_     .<<                         # type name cc buf
    sget03_ .+=
    =9_     .<<
    sget02_ .+=
    =10_    .<<
  .to_string =3 i.print_string_fd       # type name cc
  sset01 drop goto                      # };


sub prof_counter_group_code
{
  my ($name, $hashref) = @_;
  map bin qq{ "$name"
              "$_"
              lit64 >pack"Q>", $$hashref{$_}
              print_prof_counter },
      grep $$hashref{$_},
      sort keys %$hashref;
}


sub prof_counter_print_code
{
  join '', prof_counter_group_code(method   => profiled_methods),
           prof_counter_group_code(receiver => profiled_receivers),
           prof_counter_group_code(fn       => profiled_fns),
           prof_counter_group_code(macro    => profiled_macros);
}


1;
