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


=head2 Classes, protocols, and compilation
Stuff I'm currently assuming:

1. Protocols describe interfaces to classes
2. Classes own vtable generation and contain method sets and protocol lists
3. Classes are implementations, so they manage structs
4. Some classes are fixed-size, others aren't (do we want allocation protos?)
5. Class objects generate compilers that write into macro assemblers
6. vtables are prepended by C<polymorphic> ... but this is wrong
7. C<polymorphic> converts protocols to classes ... probably wrong

Here's what's broken about this picture:

1. You can't prepend stuff to a class without telling the class about it
2. C<polymorphic> won't know how much memory to reserve
3. We aren't left in a good position to have functional classes
4. It's unclear how to inline stuff, e.g. for int primitive ops

Some things that should be true:

1. C<int.+> should emit C<iplus> by itself
2. C<poly_baseptr.method> should emit C<dup m64get mcall...>
3. C<mono_baseptr.method> should emit C<lit64(vtable) mcall...>
4. It should be possible to have a pointer/vtable pair as a single logical value
5. C<poly_hereptr.method> should emit C<dup -2 + m16get - + dup m64get mcall...>
6. We need to know at parse time whether something is mono/poly
7. Meta-protocols are first-class objects that dictate interface strategy
8. Monomorphic class instances should have no stored vtable
9. Small polymorphic instances should have small/externally stored vtables

I'm also a little unclear on a few things:

1. How does class/protocol vtable slot negotiation work?
2. Can protocols own method-call codegen?
3. Are protocols just method namespaces from an allocation perspective?
4. Who manages the distinction between C<vtable> and C<reflective> etc?
5. How does the slot-0 meta-protocol negotiation work?
6. When are frames allocated, and who drives this process?


=head3 What if protocols own method call codegen?
...ignoring the protocol/class vtable negotiation for now, then we'd have
different protocol variants for monomorphic/polymorphic instances. Specifically,
let's suppose we've got something like this:

  class cons { head, tail }
  class poly_cons = cons.vtable_polymorphic

The protocols that address these classes are distinct because one expects vtable
dispatch and the other doesn't:

  cons.protocol      = { head, tail } as direct accessors
  poly_cons.protocol = { head, tail } as vtable-driven mcalls

Put differently, it isn't a question of mono/poly as much as it's a question of
dereferencing a vtable. Let's write it differently:

  class cons { head, tail }
  class vtcons = vtable_prefix(cons)

  mono_cons_protocol = cons.protocol
  poly_cons_protocol = vtcons.protocol = vtable_prefix_proto(mono_cons_protocol)

Now we have C<vtable_prefix_proto> as a proto->proto function that transforms
each method by delegating to a vtable. The difference looks like this:

  mono_cons_protocol.head = [           # &cons cc
    swap m64get                         # cc head
    swap goto
  ]

  poly_cons_protocol.head = [           # &vtcons cc
    swap dup m64get                     # cc &vtcons &vtable
    mcall <head_index>                  # cc head
    swap goto
  ]

Semantically, then, protocols own the entire mapping between a memory object and
a method call interface. Polymorphic protocols use runtime delegation on the
logic that we don't know up front what the memory layout will be, but protocols
aren't fundamentally polymorphic.


=head3 Let's simplify: no vtable compaction by default
...so we have the full class/method matrix, and each method gets a unique vtable
slot. More space, but easier/faster in general.

Then we maintain a global symbol->index table that is updated by method-name
objects at instantiation time.

Q: we'd want to compact during GC, so do we reallocate? We have to make sure
that the GC protocol itself remains stable -- so maybe those methods get
reserved values.


=head3 Caller rewriting?
Let's suppose every method call starts off using some fairly slow reflective
protocol:

  [
  "head" lit64(obj)                     # "head" obj
  dup m64get                            # "head" obj vtable
  m64get                                # "head" obj vtable[0]
  "mcall" swap call                     # call via reflective protocol
  ]
  call

...hmm, it isn't trivial to rewrite this, nor is it clear that we would want to
do it dynamically as opposed to allocating a method slot. Beyond that, who
mediates caller rewrites?

Playing this out a bit, it could be very useful to have code fragment objects be
self-managed to some extent and to have caller-directed communication (an inbox
of sorts). So I'm a function, I call into another function, and that other
function leaves something in my inbox about how to optimize stuff or something.
I'm not sure exactly how useful this would be, but it's worth keeping in mind as
a possibility.


=head3 Mono/poly and vtable lookups
How about this. We have a meta-protocol that governs how method calls are
implemented -- which we need anyway in order to do reflection -- and then we get
two layers:

  class cons { head, tail }
  protocol mono<cons>;                  # vtable is a constant closure
  protocol vtable_poly;                 # vtable is retrieved automatically

Each of these protocols goes from C<&obj> to C<&obj vtable mcall...> (or
anything else) in response to a method-invocation request. Then classes don't
store their own vtables; that's managed by C<vtable_poly> or equivalent.

Q: if C<vtable_poly> prepends data to a class, how do we manage allocation?
Here's the problem: I could have a class whose allocator created several
heap-allocated objects and then returned its pointer -- so C<vtable_poly> can't
just wrap the constructor, allocate the vtable first, and return that. It has to
somehow tell the class to prepend the correct vtable.
=cut


1;
