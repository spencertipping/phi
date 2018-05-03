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


=head1 phi optree compiler
A compiler for optrees. This translates an optree graph into concatenative code
using a series of node parsers. We could use a literal translation, but it
wouldn't be optimal; instead, this compiler optimizes a few low-level details to
produce faster code.
=cut


package phicompile;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philist;
use phidata;
use phiobj;
use phioptree;

our @EXPORT =
our @EXPORT_OK = qw/ compile /;


=head2 Compilation strategy
The goal here is to make infix code performance-competitive with concatenative.
L<phiinterp> adds a bunch of overhead to relatively simple expressions, e.g.
C<binop(+, const(3), const(4))> -- which would be much more efficiently written
in concatenative as:

  3 4 +

That's a fairly literal transcription of the optree, but not all expressions
should be compiled literally. The infix syntax introduces a lot of inline
lambdas that create a nontrivial amount of overhead of their own; let's talk
about that for a moment.

=head3 Condensing inline lambdas
C<let x = y in z> gets compiled into C<call(fn([arg=x] z), y)> -- so we have a
function we immediately invoke. A literal compilation would be:

  compile(y) [ compile(fn([arg=x] z)) ] .

There are two optimizations we can make:

1. Inline the function list to eliminate C<.>
2. Do clever things to avoid computing intermediate capture lists

Of these, (2) confers far more advantage.
=cut


1;
