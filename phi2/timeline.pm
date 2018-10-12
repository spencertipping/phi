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


=head2 Timelines
Code doesn't directly map to an evaluation schedule; there are too many degrees
of freedom to commit to a single ordering, and not all programs that are parsed
are intended to be evaluated at all. Instead, the parse output is something more
generally informative: a set of functional dependencies that describe the
calculations required to execute the code. The degree of sequencing is encoded
by the dialect and the abstracts, and collectively the calculation set is a
graph called a timeline.

Functionally speaking, timelines are phi's intermediate representation and form
a variation of CPS that specifies which expressions are temporally independent.
More broadly:

1. Timelines can be trivially converted to bytecode
2. Bytecode produced by a timeline links back to the timeline object
3. Backend-specific JIT compilers use timelines as input
4. Bytecode can be trivially converted to a GC-unsafe timeline
5. Timelines preserve/store CTTI
6. Timelines preserve/store all method calls, including monomorphic ones
7. Timelines can generate and splice more timelines as first-class values

(5) and (6) are important for algebraic analysis. Basically, the idea is that we
want timelines to be a lossless semantic encoding of the source they came from.
This means no type or intent erasure: if we have a situation where an abstract
defines two methods that compile to the same underlying function, we should
still be able to know which one was called.

(7) is how control flow works, but more generally it specifies that code isn't
confined to a static compilation model.


=head3 Timeline nodes
Timelines are made of these types of nodes:

1. C<< const(X, ctti)               -> value, ctti >>
2. C<< arg(name, ctti)              -> value, ctti >>
3. C<< method(ctti, m)              -> timeline(...) >>
4. C<< code(X, [inputs], [outputs]) -> timeline([inputs], [outputs]) >>
5. C<< call(timeline, args...)      -> v1, v2, ..., vN >>
6. C<< return(vals...) >>
7. C<< goto(timeline, args...) >>

NB: C<arg> and C<return> seem like hacks, but they aren't. C<call> and C<goto>
relink those nodes when we want to make an inline function call, and any
timeline that gets compiled into an IR node will use C<arg> and C<return> to
construct the C<ir_fn> node.

Node linkages always point towards dependencies:

  method-----------+       call-------------+
  |                |       |                |
  |ctti            |       |            ret1|<--- ...
  |        timeline|<------|timeline    ret2|<--- ...
  |method          |       |             ...|<--- ...
  |                |   +---|arg1            |
  +----------------+   | +-|arg2            |
                       | | +----------------+
          const(5)-+   | |
          |   value|<--+ |
          +--------+     |
                         |
          const(0)-+     |
          |   value|<----+
          +--------+


=head3 Connection addressing
Nodes offer multiple outputs and we'll need to know which one any given edge
refers to. This won't matter for topsorting, but it does matter for general
information management.

I think we can do something simple like maintaining two arrays:

  node* links[];
  hash  targets[];


=head3 CTTI and dialect interoperation
Let's start with everyone's favorite function:

  int factorial(int x)
  {
    int n = 1;
    while (x > 1)
    {
      n = n * x;
      x = x - 1;
    }
    return n;
  }

Here's how it looks in timeline terms, where C<t0>, C<t1>, ..., are sequence
argument placeholders. Every C<call> node provides a C<seqarg'> return, but I've
elided those connection points when they're unused.

  factorial = timeline(int)
  {
    arg(0) <-- t0

    # Create a frame to store locals for this function...? If we do this,
    # though, then timelines won't be inlinable.

                                call--------------+
    const(code_create_frame) <--|timeline         |
    t0 <------------------------|seqarg    seqarg'|<------t1
                                +-----------------+

    # Sequence the first set of real operations after the frame code...
    # This is all wrong.

                                call--------------+
    const(code_setlocal_n) <----|timeline         |
    t0 <------------------------|seqarg    seqarg'|<------tneq
    const(1) <------------------|arg0             |
                                +-----------------+

                                call--------------+
    const(code_setlocal_x) <----|timeline         |
    t0 <------------------------|seqarg    seqarg'|<------txeq
    arg(1) <--------------------|arg0             |
                                +-----------------+

    # Sequence point: merge all sequence arguments to get the next one. This
    # doesn't create any code, nor are sequence arguments real values -- the
    # sole purpose of this is to make sure we get the right ordering from graph
    # topsorting.

                                call--------------+
    const(nop) <----------------|timeline         |
    t0 <------------------------|seqarg    seqarg'|<------t1
    tneq <----------------------|arg0             |
    txeq <----------------------|arg1             |
                                +-----------------+


    while_cond = timeline
    {
      arg(0) <-- tcond0

      # NB: the below isn't quite correct; in reality we keep the seqarg from
      # the getlocal_x call and use a nop merge before the return.

                                               call------------+        return+
                        const(code_int_lt) <---|timeline       |        |     |
                        tcond0 <---------------|seqarg  seqarg'|<----+--|seq  |
                                call--------+  |           ret0|<-+  |  |     |
      const(code_getlocal_x)<---|timeline r0|<-|arg0           |  |  +--|ret0 |
      tcond0 <------------------|seqarg     |  |               |  |     |     |
                                +-----------+  |               |  +-----|ret1 |
                        const(1) <-------------|arg1           |        +-----+
                                               +---------------+
    }


    while_body = timeline
    {
      arg(0) <-- tbody0                 # NB: initial sequence point

                           call---------+
      const(code_get_n) <--|timeline  r0|<----nval
      tbody0 <-------------|seqarg   sa'|<----tgetn
                           +------------+

                           call---------+
      const(code_get_x) <--|timeline  r0|<----xval
      tbody0 <-------------|seqarg   sa'|<----tgetx
                           +------------+

                           call---------+
      const(code_imult) <--|timeline    |
      tbody0 <-------------|seqarg    r0|<----new_nval
      nval <---------------|arg0     sa'|<----tmult
      xval <---------------|arg1        |
                           +------------+

      # We don't use the return value from this call, so nothing connects to its
      # r0 output:
                                 call--------+
      const(code_setlocal_n) <---|timeline   |
      tbody0 <-------------------|seqarg  sa'|<---tneq
      new_nval <-----------------|arg0       |
                                 +-----------+


                          call-------------+
      const(nop) <--------|timeline        |
      tbody0 <------------|seqarg       sa'|<----tbody1
      tgetn <-------------|arg0            |
      tgetx <-------------|arg1            |
      tmult <-------------|arg2            |
      tneq <--------------|arg3            |
                          +----------------+

      # Now tbody1 is the sequence point marker.
      ...
    }

    TODO: ...
  }


I have some questions about the above:

1. How do function frames get created? (It happens after optimization, clearly.)
2. Do C<call> nodes report the CTTIs of their results?
3. Are all local jumps encoded as C<goto>s?
4. Are C<timeline> containers marked as being functions?

(4) means I have a problem. Let's get into this.


=head3 Function boundaries and frame allocation
This comes down to local variable access, and we have some constraints:

1. Lexical/dynamic scoping differences mean we can't push scope into timelines.
2. We can't have opaque frame allocations because that prevents inlining.
3. We can't commit to a frame prior to optimizations.


=head3 Node aliasing in general
Object identity matters, so we can't split arbitrary nodes during optimization.
This means we need to know the full forward set from any given node.


=head3 Optimization mechanics
First, timeline rewrites aren't lossy. Rewritten timelines store C<source>
pointers that refer to the input of the rewrite in question; this makes it much
easier to debug things. Timeline nodes aren't entirely immutable, but their
canonical representation is. I'll explain this in more detail below.

Optimization parsers often look for multilayer structures, most of which won't
match due to some property of a child node. For example, many CTTIs define
parsers that will match C<call(method(...), args...)> and replace that with an
inlined sub-timeline. Ideally we can reject mismatching C<call> nodes without
inspecting C<method> directly.

We have 64 bits to work with and we want some ability to mask stuff,
particularly specific arguments for function call matching. In particular:

1. C<goto> and C<return> are never linked to, so they have no hashes
2. C<const>, C<call>, and C<method> are things we want to identify by type
3. C<method> is identified by CTTI and by name
4. C<call> always matches arity, and arguments can match type and structure
5. C<arg> and C<code> should cause many parsers to fail

Let's start with three bits for the type:

  000 = const
  001 = method
  010 = call arity == 1
  011 = call arity == 2
  100 = call arity == 3
  101 = call arity == 4
  110 = call arity >= 4
  111 = arg|code

What happens next depends on which type of node we have. C<method> has a couple
of indicator bits to specify whether the CTTI and method name are C<const>
nodes, followed by a hash that varies accordingly (here, C<hashN(X)> refers to
the most-significant C<N> bits of C<hash(X)>):

  method(const(type), const(mname)) -> 001 11 (hash59(type) ^ hash59(mname))
  method(type, const(mname))        -> 001 01 hash59(mname)
  method(const(type), mname)        -> 001 10 hash59(type)
  method(type, mname)               -> 001 00 hash27(type) hash32(mname)

C<call>'s hashing structure depends on its arity, and we have an indicator bit
for the const-ness of the function.

  call(const(f), ...)                          -> XXX 1 ...
  call(method(const(type), const(mname)), ...) -> XXX 1 ...
  call(non-const, ...)                         -> XXX 0 ...

  call(X, a)                -> 010 Xc? hash28(X) hash32(a)
  call(X, a, b)             -> 011 Xc? hash20(X) hash20(a) hash20(b)
  call(X, a, b, c)          -> 100 Xc? h15(X) h15(a) h15(b) h15(c)
  call(X, a, b, c, d)       -> 101 Xc? h12(X) h12(a) h12(b) h12(c) h12(d)
  call(X, arity<=19, xs...) -> 110 Xc? (arity-4) h12(X) (h[arity//44](x) for xs)

Note that this hashing strategy is just to get things off the ground. I think we
can derive a more efficient one based on the set of patterns we're looking for,
but that's a phi3 thing.
=cut


1;
