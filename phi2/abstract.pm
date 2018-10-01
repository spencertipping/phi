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


=head2 Abstracts
There are a few big questions we need to answer about how abstracts work:

1. Do abstracts represent values-at-locations, or just values?
2. Is an abstract value aware of its timeline?
3. How do we go from abstracts to compiled code?
=cut

use phi::protocol abstract_ctti       => qw/ ctti /;
use phi::protocol abstract_evaluation => qw/ method /;


=head3 Residency
Abstracts are aware of their residency. The workflow is always to convert
between not-stack and stack -- the idea being that values carry lvalue-ness when
they aren't stacked. Put differently, stack values aren't subject to things like
alias or memory analysis. A stack value is as pure as it gets.

This means that non-stack abstracts will typically return a stacking wrapper
when you want to work with them, and the stacking wrapper contains all the logic
for working with the value. For example:

  x = 5;            # "x" -> abstract_local("x", abstract_int_const(5))
  (x + 1).print;    # abstract_local("x", ...) .stack()
                    #  = abstract_int_const(5) .method("+",     [abstract_int_const(1)])
                    #  = abstract_int_const(6) .method("print", [])
                    #  = abstract_void

Local assignments involve collapsing side effects; that is, we need to manage
continuations to get strict evaluation semantics. As a result, if C<x = ...>,
C<x> and C<...> are two distinct abstracts:

  x = y + 1;        # y.stack().method("+", [1]).do() -> abstract_unknown_int(...)
  x;                # abstract_local("x", abstract_unknown_int(...)).stack()

Abstracts provide methods to make themselves GC-atomic. Some abstracts like
integers don't need anything special for this to happen; it's fine to leave ints
on the stack because they're GC-inert. But other things like pointers will write
themselves into temporary locals so the active frame can trace them.

API:

  abstract.ctti()                -> ctti
  abstract.stack()               -> abstract'
  abstract.as_local("name")      -> abstract'
  abstract.as_gc_atomic()        -> abstract'
  abstract.local_cttis_into(map) -> map

=cut

use phi::protocol abstract_residency =>
  qw/ stack
      as_local
      as_gc_atomic
      local_cttis_into /;


=head3 Side effects and timelines
An abstract value doesn't own its history nor its dependencies. That is, if we
have something like C<x = 6; x + 1>, C<x + 1> doesn't hold a reference to C<6>
unless C<x> is bound to the constant -- and that happens only when the two
statements happen inside the same basic block.

This is kind of a subtle point. Basically, abstracts themselves aren't aware of
their timeline context at construction-time.

Perhaps counterintuitively, abstracts _are_ aware of their impact on various
timelines -- and there are two. One timeline is called C<gse> (global
side-effects) and must be exactly ordered; this is for events that are
observable by the kernel. The other timeline, C<lse>, is for process-level side
effects like memory reads. It's ok to drop and/or reorder events from this
timeline in some cases.

C<do()> reduces a value to a read event against C<lse>; all write events are
collapsed. Importantly, there's no guarantee about stack-resident computation;
there may be reasons for phi to incur an access overhead that involves more
stack instructions than you might expect to get to a local variable, for
example. This may sound useless, but it gives phi a way to apply high-level
optimizations to values that have been assigned to variables.

Another way to think about it is that abstracts with no LSE or GSE impact are
constant transformations of a value, and abstracts with LSE or GSE interaction
at all are constant values. We may have reasons not to calculate those constants
at compile-time, but they are knowable.
=cut

use phi::protocol abstract_timelines =>
  qw/ do
      impacts_gse?
      impacts_lse?
      references_lse? /;


=head3 Linking and compilation
How do we get a program from abstracts that don't track their history? We ask
the abstract to compile itself with a specified history, a process that's driven
by combiner abstracts. The benefit here is most obvious for inlined function
calls, but it can also be used to eliminate locals and optimize through some
C<lse> events.

Implicit in the above is that linking works backwards: when we compile something
like C<x = 5; x + 1>, C<x + 1> receives C<x = 5> as its predecessor. This
mechanism does a few things for us:

1. It makes dead code trivial to detect, for the most part
2. It allows abstracts to apply dynamically-scoped optimizations
3. It allows us to rearrange conditionals and loops
4. We can separate computation from side effects
5. We can factor timelines across value chains

Here's the API:

  abstract.compile(frame_offsets, prior_abstract|0, asm) -> asm


=head3 Optimization levels
phi has two opportunities to optimize. High-level optimization is driven by
abstracts and happens at link-time. Libraries are responsible for implementing
appropriate high-level optimizations that involve semantic knowledge. This is
sort of like C++ template specialization, but more so.

C<hlopt> produces bytecode, which is then subject to low-level optimization.
This isn't owned by abstracts; instead, it's a core phi library that models the
interpreter state and applies abstract optimization at the bytecode level. Put
differently:

  hlopt: abstract -> bytecode [optimize using abstract semantics]
  llopt: bytecode -> bytecode [optimize using bytecode semantics]
  jit:   bytecode -> ???      [optimize using backend semantics]

=cut

use phi::protocol abstract_compilation => qw/ compile_into /;


use constant abstract_protocols =>
  [ abstract_ctti_protocol,
    abstract_evaluation_protocol,
    abstract_residency_protocol,
    abstract_timelines_protocol,
    abstract_compilation_protocol ];


1;
