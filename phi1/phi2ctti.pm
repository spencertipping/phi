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


=head2 CTTI for phi2
Ultimately our goal is to build up to a full set of self-hosting CTTI instances
for phi2. We don't need to define that full set here, though; we just need to
get enough stuff off the ground that we can use the frontend syntax to get the
rest. Before I get into the details, though, let's discuss bootstrap strategy.

A few things are true at this point:

1. Anything we write in phi1 will get rewritten in the phi2 boot code
2. ...therefore, we should minimize the phi1->phi2 support
3. Any nontrivial bytecode we write by hand is GC-unsafe
4. Any composite data structures we write manually will get rewritten
5. phi2 CTTI needs to target both the phi1 jurisdiction and its own RTTI linkage

...so with that in mind, let's design the set of CTTI instances required to get
phi2 off the ground.


=cut


1;
