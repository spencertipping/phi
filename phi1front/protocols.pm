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


=head2 Frontend protocols
This is a good opportunity to discuss how the frontend works at a high level.
Let's start with the typed macro assembler, which is where the handoff between
backend and frontend happens.

As we're parsing, we maintain two things: the semantic state (the typed
assembler) and the syntactic state (the regular parse state). These two interact
in a few ways:

1. Dialects: semantic -> syntactic
2. OOB continuations: semantic -> syntactic
3. Scope/bindings: syntactic -> both

I'll go through these one by one in a moment, but first let's talk about the
syntactic parse state and parsers in general.


=head3 Parsers in general
Nothing about phi's frontend demands that we parse text -- you could write a
dialect that interacted with any other format easily enough -- but phi2 is
written in text so it's worth defining a parser library for it. The usual
suspects in parsing expression grammars, plus some transforms for computed
elements (implementations in L<phi1front/parsers.pm>):
=cut

use constant parse_position_protocol => phi::protocol->new('parse_position',
  qw/ fail? /);

use constant linear_position_protocol => phi::protocol->new('linear_position',
  qw/ +
      index /);

use constant parser_protocol => phi::protocol->new('parser',
  qw/ parse /);

use constant string_parser_protocol => phi::protocol->new('string_parser',
  qw/ text /);

use constant binary_parser_protocol => phi::protocol->new('binary_parser',
  qw/ left
      right /);

use constant seq_parser_protocol => phi::protocol->new('seq_parser',
  qw/ combine
      combiner /);

use constant char_parser_protocol => phi::protocol->new('char_parser',
  qw/ chars /);

use constant repeat_parser_protocol => phi::protocol->new('repeat_parser',
  qw/ mincount /);

use constant parser_transform_protocol => phi::protocol->new('parser_transform',
  qw/ parser /);

use constant fn_parser_protocol => phi::protocol->new('fn_parser',
  qw/ fn /);


=head3 Syntactic state
A syntactic state is a regular string-compatible parse state that also tracks
the following:

1. A stack of dialects (the topmost is active)
2. A scope chain
3. An operator precedence/masking indicator
4. A semantic state (the typed asm)
=cut

use constant syntactic_state_protocol => phi::protocol->new('syntactic_state',
  qw/ dialects
      dialect
      scope
      opgate
      with_opgate
      asm /);


=head3 Dialects
A dialect provides parsers for a CTTI on the typed assembler stack. The CTTI
isn't generally aware of this, although there is a backchannel made available to
CTTIs to specify out-of-band parse continuations that should be inlined into
grammars regardless of dialect.

API-wise, dialects are just regular parsers that happen to interact with a
semantically-aware parse state.

The CTTI backchannel protocol is exactly the same thing: CTTI elements are
parsers. Most of the time they will fail the parse, indicating that they have no
dialect-independent continuation.


=head3 Scope chain
Scope chains aren't entirely straightforward because they need to not only
resolve variables, but also relay information about variable capture. Scope
chains themselves don't compile anything; they just track a set of bindings.
=cut

use constant scope_protocol => phi::protocol->new('scope',
  qw/ locals
      captures
      contains?
      local=
      local_class
      pulldown
      parent
      child /);


=head3 Operator gating
This serves two purposes. First, we gate to restrict operator precedence as
we're parsing; for example, let's suppose we have something like this:

  3 + 4 * 5 + 6
           ^
           parse position is here

At this point C<4> owns the parse, but it can't continue and grab the C<+ 6>; if
it did, the parse tree would be lopsided and incorrect. Instead, 4's parse
should fail and fall back to C<3 + 4 * 5>, which can then resume and get C<+ 6>
at the correct precedence.

Second, we gate to allow syntactic constructs to shadow specific operators. This
is kind of a dumb/elegant workaround for irregular grammars like OCaml's
C<match> construct, which uses C<|> to indicate alternatives. I didn't want to
rule out C<|> as bitwise-or, so I introduced operator shadowing to be able to
support both within the same grammar.

Shadows selectively block specific named operators independently of the active
precedence. They're coupled with precedence because ambiguity typically persists
only within some precedence level (for instance, once you're lower-precedence
than a C<match> operator in OCaml, C<|> stops applying).

phi models precedence bidirectionally: you can have an operator whose
left-facing precedence differs from its right-facing precedence. Perl exhibits
this behavior if you call a multary function without parentheses:

  3 + f 4, 5        # == 3 + f(4, 5) if f has a multary prototype

Perl's model is a bit strange in that it treats C<f> as an operator, but the
basic idea of asymmetric precedence still holds.
=cut

use constant op_protocol => phi::protocol->new('op',
  qw/ name
      precedence /);

use constant op_precedence_protocol => phi::protocol->new('op_precedence',
  qw/ left
      right
      right_associative?
      postfix?
      binds_rightwards_of? /);

use constant op_gate_protocol => phi::protocol->new('op_gate',
  qw/ precedence
      shadowed
      parent
      child_for
      allows? /);


=head3 Semantic state and CTTI residence
Values must be stored in the frame structure in order to be GC-atomic, which
raises a couple of issues:

1. What do local scopes resolve to? (i.e. not just CTTI instances)
2. How do we generate the frame-allocation code if the class is parse-dependent?

Let's tackle (1) first.

A local scope can't simply bind C<x> to C<int_class> because that doesn't tell
us how to retrieve the int from the current frame and push it onto the stack.
C<int_class> instances themselves aren't aware of their residence, so what we
really want is for the scope to bind C<x> to a location-aware CTTI. Luckily
struct fields provide this for us: each field has a CTTI type definition.

NB: using struct-field CTTI erases state knowledge within basic blocks. We can
do better in some cases by propagating abstract invariants, but it gets
complicated and is beyond the scope of phi1.

(2) is a bit more subtle than CTTI residence and involves some light macro
assembler trickery.

Parsers will assemble things into a function body as the function body is being
parsed, but we don't yet know the extent of local variables. Here's an example
of some code where this is true:

  int main()
  {
    int x = 10;
    printf("%d\n", x);          // this is compiled at parse-time
    int y = 20;                 // ...but this modifies the frame class
    return x + y;
  }

...so we effectively need a buffered region of code that we can patch in after
we're done parsing and compiling the function body.

The simplest way to do this is to assemble the function body into a child
assembler and then link it back to the parent with a C<goto>. So we'd have this:

  asm                           # parent
    dup .[                      # parent child
    <fnbody-asm>                # parent child'
    swap                        # write destructively into parent asm
    <fnframe-asm>               # child' parent'
    drop                        # ...switch back to child
    .] .goto                    # parent''

The result looks like this:

  fnframe [ fnbody ] goto

The requirement here is that C<fnframe-asm> not create any new assembler
objects; all updates need to be in-place destructive calls against C<parent>.


=head3 GC atomicity for temporary values
If we have an expression like C<foo + bar + bif>, C<foo + bar> needs to be saved
somewhere in order to be GC-atomic for C<+ bif>, which could allocate memory.
There are two ways we could do this:

1. Have the calling frame allocate a temporary for C<foo + bar>, then clear it
2. Make a rule that the callee must save anything it relies on

Of these, (2) makes a lot more sense. First, there's no reason that an argument
needs to survive for the entire duration of a function; if the function drops
the reference, then it should be free to be collected.

Second, temporaries create a lot of complexity for the caller to manage. It's
far simpler to have the callee stash its arguments into a frame and call it a
day.


=head3 Parse rejection and C<alt> failover
A parse can fail at any point, which creates a problem if we're destructively
updating our macro assemblers.

TODO: figure out how to manage this

=cut


1;
