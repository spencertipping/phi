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

...so with that in mind, let's design the set of CTTI instances required to get
phi2 off the ground.


=head3 Compiled code
Let's take a simple function like C<rev>, which reverses a list. Here's what the
source might look like:

  rev xs t = xs.nil?
    ? t
    : rev xs.tail (xs.head :: t)

The simplest tail-recursive concatenative design looks like this (assuming a
required second arg of nil):

  rev = [                               # xs t cc
    [ sget02 .nil?                      # xs t cc loop nil?
      [ drop sset01 swap goto ]         # t
      [ sget02 sget04 .head :: sset02   # xs  x::t cc loop
        sget03 .tail sset03             # xs' x::t cc loop
        dup goto ]                      # ->loop
      if goto ]                         # xs t cc loop
    dup goto ]                          # ->loop

This design isn't GC-atomic, though, which it needs to be given that it
allocates cons cells. To fix this, we need to allocate frames. That code uses a
function prologue and looks like this:

  rev = [                               # xs t cc
    get_frameptr                        # xs t cc f

    [                                   # xs t cc f loop vt|
      get_frameptr .xs .nil?
      [ get_frameptr .t                 # |t
        get_frameptr .ret ]             # t (unwinds the frame)

      [ get_frameptr .t                 # |t
        get_frameptr .xs .head          # |t x
        ::                              # |x::t
        get_frameptr .t=                # |

        get_frameptr .xs .tail          # |xs'
        get_frameptr .xs=               # |

        # Reuse the same frame and tail-call
        get_frameptr .loop goto ]       # ->loop

      if goto ]                         # xs t cc f loop

    $rev_frame_vtable                   # xs t cc f loop vt
    get_stackptr set_frameptr           # xs t cc f loop vt|

    get_frameptr .loop goto ]           # ->loop

The above is a bit of a lie in that we actually don't make any polymorphic
method calls since we already know the vtable's class (i.e. we can inline all of
the methods) -- but otherwise that's what's going on. We do need the frame
vtable either way because the GC requires it. After inlining, we end up with
this:

  rev = [                               # xs t cc
    get_frameptr                        # xs t cc f
    [                                           # xs t cc f loop vt|
      get_frameptr lit8+40 iplus m64get .nil?
      [ get_frameptr lit8+32 iplus m64get
        get_frameptr lit8+40 iplus m64set       # t t cc f loop vt|
        drop drop set_frameptr                  # t t cc
        sset00 goto ]                           # t
      [ get_frameptr lit8+32 iplus m64get
        get_frameptr lit8+40 iplus m64get
        .head ::
        get_frameptr lit8+32 iplus m64set       # xs  x::t cc f loop vt|
        get_frameptr lit8+40 iplus m64get
        .tail                                   # xs' x::t cc f loop vt|
        get_frameptr lit8+40 iplus m64set
        get_frameptr lit8+8  iplus m64get
        goto ]                                  # ->loop
      if goto ]                                 # xs t cc f loop

    $rev_frame_vtable                   # xs t cc f loop vt
    get_stackptr set_frameptr           # xs t cc f loop vt|
    get_frameptr lit8+8 iplus m64get    # xs t cc f loop vt| loop
    goto ]                              # ->loop

Most of the structure in this function is identical to the concatenative
version, but uses frame memory offsets instead of C<sget>/C<sset>. In
particular, the tail-recursive loop will JIT to exactly the same number of
operations in both implementations; there's no net overhead from using OOP to
generate our code (and we have an upside in that it's GC atomic).
=cut


1;
