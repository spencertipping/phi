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


=head1 phi abstract interpreter
Simulates the concatenative interpreter used to execute phi programs. In doing
so, we can aggressively fold constants to get partial evaluation, which should
in turn yield specialized/optimized concatenative code. This is a prerequisite
for higher-level abstractions, whose unoptimized versions run far too slowly to
be useful.

I think this concept is the same thing as Futamura projection and partial
evaluation. It may differ a bit because phi is built around immutable objects
(or objects that can easily be modeled in immutable terms), so I use some
different terminology here. I also haven't read the Futamura paper or anything
so my design might differ arbitrarily.

The core idea here is that we reduce the number of concatenative operations
required to do something, in our case by simulating the evaluator applied to
partially-constant expressions. Those degrees of specification allow us to
eliminate certain conditional branches and allocation. For example, most of the
method calls made against objects involve clearly-redundant linear scans to
resolve symbols to method implementations. We can pre-resolve and ultimately
inline any monomorphic call site.

Rather than applying specific optimizations, we simulate the entire interpreter
state as a single abstract value and treat it as a system that needs to be
reverse-engineered. For example, if we have a function C<f> that we know takes
and returns two integers, we create two unknown quantities C<x> and C<y>, push
them on the stack, and then measure the interpreter after applying C<f>. We then
have a (presumably) faster pseudo-instruction that will behave identically to
C<f>.


=head2 Abstract values and tracing
Let's start with simple immutable values. TODO


=cut


package phiabstract;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philist;
use phiparse;
use phiobj;


1;
