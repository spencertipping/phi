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


=head2 Test execution
Just a simple wrapper for test functions. In particular, I want to measure the
runtime and possibly the incremental heap usage of each one. Usage is like this:

  $test_fn "test function name" test

The test function must take and return no arguments.
=cut

use phi::fn test => bin q{              # f name cc
  sget01      =2 i.print_string_fd      # f name cc
  ": "        =2 i.print_string_fd      # f name cc
  $ansi_clear =2 i.print_string_fd      # f name cc

  i.heap_usage                          # f name cc h0
  micros                                # f name cc h0 t0
  sget04 call                           # f name cc h0 t0
  micros                                # f name cc h0 t0 t1
  swap ineg iplus                       # f name cc h0 dt
  swap i.heap_usage                     # f name cc dt h0 h1
  swap ineg iplus                       # f name cc dt dh

  strbuf                                # f name cc dt dh sb
    =27_ .<<
    "[60Gm"_ .+=
    =27_ .<<
    "[J"_ .+=
    .<<dec                              # f name cc dt sb
    =27_ .<<
    "[70Gt"_.+=
    .<<dec                              # f name cc sb
    .to_string i.pnl_err                # f name cc

  sset01 drop goto                      # };


sub test_runner_code
{
  join '', map bin qq{ lit64 >pack "Q>", test_fns->{"$_"}
                       "$_ tests" test },
               @{+test_list};
}


1;
