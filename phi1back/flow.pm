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


=head2 Continuation flow
phi's compiler consists of a series of stack conversions, each adapting the set
of arguments passed into the continuation of the current expression. This set of
arguments is called the "refset," and we care about it because we end up using
it to generate the object stored in the frame pointer. This object is the GC
root.

Call frames can be stack or heap-allocated; the interpreter doesn't need to know
the difference. The frame object will handle GC slightly differently depending
on which it is (if stack-allocated, it won't try to write itself into the new
heap).

Flow assemblers are linked structures that proceed leftwards, unlike lists which
proceed rightwards. That is, suppose we want the following sequence:

  push_frame(X) update_frame(Y -> Z) pop_frame(Q)

The flow assembler will look like this:

  pop_frame_link(Q, tail =
    update_frame_link(Y, Z, tail =
      push_frame_link(X, tail =
        nil_flow_assembler)))

=cut


use constant flow_assembler_protocol => phi::protocol->new('flow_assembler',
  qw/ into_asm /);


use constant nil_flow_assembler_class => phi::class->new('nil_flow_assembler',
  maybe_nil_protocol,
  flow_assembler_protocol)

  ->def(
    "nil?"   => bin q{=1 sset01 goto},
    into_asm => bin q{sset00 goto});


=head3 CTTI
Every language represents values as some mixture of CTTI (compile-time type
information) and RTTI (runtime type information), typically split as dictated by
the typing discipline. phi doesn't impose a global type system, so you have
fine-grained control over the line between compile-time and runtime.

Full evaluation is the upper bound of compile-time knowledge; at that point
there is no RTTI at all and we reduce a program to a constant or something
similarly trivial. If we refuse to follow function calls or constant-fold
conditionals, then we get a basic bottom-up type inference algorithm. If we do
no evaluation at all, we end up with something like Smalltalk: all data about
values is available as RTTI.


=head3 Basic blocks and linking
Each flow assembler behaves like C<bin> without support for C<[> and C<]>: it's
an uninterrupted chunk of code that runs from start to end. We can build
C<[...]> blocks by referring to other flow assembler objects. This is done using
a custom link-a-block link.


=head3 Stack/frame interfacing
Flow assemblers are made up of links, which fall into a few categories:


=head4 C<push_frame>
We need to know two things to create a frame:

1. The full set of ref IDs and their CTTIs
2. The incoming stack layout, in terms of this new refset

Frame classes in general are generated and managed by the flow assembler; this
coupling exists because the flow assembler is responsible for moving values
between the stack and the frame, which entails addressing the object somehow.

Q: is it worth designing this in terms of metaclasses or protocols or something?
We also need to tell the interpreter how to address the frame pointer for GC
purposes, so the abstraction escapes flow assemblers.

Here's the struct for this link:

  struct flow_push_frame_link
  {
    hereptr        class;
    flow_asm*      tail;
    strmap<ctti*>* frame_layout;
    list<string*>* stack_layout;
    class*         frame_class;
  }

=cut


use constant flow_push_frame_protocol => phi::protocol->new('flow_push_frame',
  qw/ frame_layout
      stack_layout
      frame_class /);


use constant flow_push_frame_class => phi::class->new('flow_push_frame',
  maybe_nil_protocol,
  cons_protocol,
  flow_assembler_protocol,
  flow_push_frame_protocol)

  ->def(
    "nil?"       => bin q{=0 sset01 goto},
    head         => bin q{goto},
    tail         => bin q{swap =8  iplus m64get swap goto},
    frame_layout => bin q{swap =16 iplus m64get swap goto},
    stack_layout => bin q{swap =24 iplus m64get swap goto},

    frame_class  => bin q{              # self cc
      sget01 =32 iplus dup m64get       # self cc &c c?
      [ m64get sset01 goto ]            # c
      [ "TODO: generate frame class" i.die ]
      if goto                           # c },

    into_asm     => bin q{              # asm self cc
      sget02 sget02 .tail .into_asm     # asm self cc asm
      drop sget01 .frame_layout .length # asm self cc frame-slots

      # Stack-allocate the frame object. We'll need two additional slots, one
      # for the original frame pointer and one for the new frame's class
      # hereptr.
      #
      # The new frame lives below the stack pointer; i.e. there's no overlap.
      #
      # As an instance, the frame object is built of stack cells we initialize
      # to neutral values (where "neutral" is ultimately specified by the CTTI,
      # but for now we'll just use 0).
      =0                                # asm self cc fn i
      [                                 # asm self cc fn i loop cc
        sget03 sget03 ilt               # asm self cc fn i loop cc i<fn?
        [ sget06 .lit8 =0 swap .l8 drop # ... fn i loop cc
          sget02 =1 iplus sset02        # ... fn i+1 loop cc
          sget01 goto ]                 # ->loop
        [ sset01 drop goto ]            # ->cc
        if goto ]                       # asm self cc fn i loop
      dup call                          # asm self cc fn

      # Now we've added all of the frame data. We just need to push the original
      # frame and the new frame's class dispatch function. The dispatch function
      # is a hereptr to a class object.
      sget03 .get_frameptr              # asm self cc fn asm [get_frameptr]
      sget03 .frame_class .dispatch_fn
      swap .hereptr                     # asm self cc fn asm [... $dfn]
      .get_stackptr .set_frameptr       # asm self cc fn asm

      # Now grab the original entries and populate the frame data. We might have
      # enough frame cells that the original stack is beyond the one-byte offset
      # range of sget, so we need to use m64get rather than stack accessors.
      #
      # Here's how this works. We calculate the original stack pointer, then
      # emit a series of (dup =8 iplus swap m64get f .field=) sequences to
      # populate frame elements.

      swap =3 ishl swap                 # asm self cc fsize asm
      .get_frameptr .lit16 .l16 .iplus  # asm self cc asm [...f &s]

      # Now iterate through the stack entries. These are sequential, in our case
      # referring to successive cells beginning inclusively with &s.

    });


=head4 C<update_frame>
Modifies one or more values stored in the refset. This link stores three things:

1. Initial stack layout, in terms of ref IDs
2. Concatenative code
3. Final stack layout, in terms of ref IDs

NB: it doesn't make sense for C<update_frame> links to be able to modify the
CTTI of a given slot. This introduces a dependency on control flow, which means
we're dealing with RTTI not CTTI.


=head4 C<link_code>
This stores a flow assembler pointer and gives you a way to add a code-hereptr
to the compiled result of a separate flow assembly. In other words, this makes
it possible to link multiple flow assemblers together.

C<link_code> produces a value compatible with the C<< hereptr<bytecode> >> CTTI.
That is, you can use it directly with a C<goto> or C<call> instruction, but the
result is addressable as an object.


=head4 C<pop_frame>
Flattens selected frame entries onto the stack and restores the parent frame
object. All we store is the final stack layout.


=head4 C<concatenative>
Adds some literal concatenative code, with the implied promise that the frame
isn't modified or used. This is often used to issue the final C<goto>
instruction after a C<pop_frame> link. Technically there's nothing stopping us
from using C<update_frame> for this, but that would be a little bit dishonest
because C<update_frame> makes no promises about the stack layout (and in fact it
expects the stack to be empty before+after itself). A C<concatenative> node
addresses the stack directly.
=cut


1;
