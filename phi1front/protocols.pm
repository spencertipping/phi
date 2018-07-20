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
4. A semantic state
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

use constant scope_chain_protocol => phi::protocol->new('scope_chain',
  qw/ local{}
      capture{}
      contains?
      pulldown
      parent
      child
      local= /);


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

=cut


1;