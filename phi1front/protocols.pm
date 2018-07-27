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
semantically-aware parse state. They get an empty protocol.

The CTTI backchannel protocol is exactly the same thing: CTTI elements are
parsers. Most of the time they will fail the parse, indicating that they have no
dialect-independent continuation.

CTTIs negotiate with dialects to specify things like supported methods and
operators, and possibly other attributes like primitive type mappings. We have
the usual many-to-many problem, so let's get into how this works.

=head4 Open-ended dialect negotiation
At a high level, CTTIs opt into various negotiation channels, of which there are
a few defined up front:

1. C<op>: "I provide operators"
2. C<method>: "I provide methods"
3. C<parser>: "I provide a custom parse continuation"

Here's an example dialect/CTTI negotiation process:

  dialect.parse(in, pos)
    -> CTTI.dialect_channels()          = ["op", "method"]
    -> CTTI.inspect_channel("op")       = {"+" -> rhs_parser,
                                           "-" -> rhs_parser,
                                           ...}
    -> CTTI.inspect_channel("method")   = {"to_s" -> signature,
                                           "size" -> signature,
                                           ...}

CTTIs can opt out of channels, and in some cases they should. For instance, the
C<unknown> CTTI used by typed assemblers doesn't correspond to a value we can
reliably address in any given way, so it should indicate that you can't do
anything with it by providing no channels to the dialect.
=cut

use constant dialect_protocol => phi::protocol->new('dialect');

use constant dialect_negotiation_protocol =>
  phi::protocol->new('dialect_negotiation',
    qw/ dialect_channels
        inspect_channel /);


=head3 Scope chain
phi supports scopes with parse-time lexical capture. You can also do dynamic
capture if you want to, but that's up to the dialect to implement. Lexical
capture requires some parse-time support (and is more common/useful), so that's
what scopes are built to simplify.

The dialect plays a role in translating the scope into something that can be
parsed. There are three big reasons this needs to happen:

1. Some languages (e.g. Perl) use dialect-specific sigils/prefixes
2. Some languages (e.g. Python, Javascript) use bizarre scope capture rules
3. Some languages (e.g. C++) use capture-style things for type names

Let's discuss these in more detail.

=head4 Prefixes
Here's the problem I need to avoid:

  in python:
    x = [10]

  in perl
  {
    push @$x, 20;
    print "@x\n";               # @x is undefined
    print "%x\n";               # %x is undefined
  }

The above behavior is what we want. Perl's hashes-as-values and arrays-as-values
don't translate easily into most other languages, so we can keep it simple and
define a mapping for scalar values without binding anything else. That is:

  in perl
  {
    our %x = (foo => "bar");    # this value isn't exported...
    our $y = \%x;               # ...but this one is
  }

  in python:
    x["foo"]                    # should fail: perl doesn't export anything
    y["foo"]                    # this should work

I may change this a little to have Perl export multichannel fused values when
name overlaps exist, but the overall gist is that dialects are at liberty to
functionally transform the underlying name/value mappings.

=head4 Wonky scoping
Language developers are awful people, and I cite as evidence abominations like
the following:

  def foo(x):
    y = 10                      # this local-looking variable...
    return y                    # ...which we can work with freely...
    global y                    # ...is actually a global (with a warning)

  var f = function ()
  {
    var g = function ()
    {
      return x;                 // this...
    };
    var x = 10;                 // ...closes over this
    return g;
  };

None of this is much of a problem if parsing and scope analysis are two separate
phases; but phi's scopes resolve at parse time in order to provide interpolated
grammar. So how do we accommodate these broken languages?

The basic problem here is that our scope chain is unspecified while we're
parsing things, and we don't have the ability to retroactively reparse if we
discover stuff (nor would we want to, for any number of reasons). To their
credit, these languages don't demand that we would; they have constant grammars.
But phi adds interpolation, which isn't really compatible with this model. This
means two things:

1. The "real Python" dialect must parse captured values as generics
2. Scopes need a way to capture first, resolve/specify later

(2) doesn't need to change any performance characteristics, beyond the
opportunity cost of losing CTTI-specific inlines when we get stuck with
generics. The inner scope will see a captured local either way; it's up to
whoever's instantiating that scope to drop the captured value in. For code like
the nested JS functions above, C<x> will be promoted into a local in C<f>'s
scope and assigned at the C<var x> line; the resulting scope chain will end up
being equivalent to this:

  var f = function ()
  {
    var x;
    var g = function ()
    {
      return x;
    };
    x = 10;
    return g;
  };

In other words, because values must be captured into a local scope from
somewhere, any reference to a captured quantity will always just end up being a
local access against the current frame pointer. The only question is how they
got there, but that's up to the lexical parent (which itself will refer to
locals to fill the capture slots).

=head4 Selective/non-value capture
C++ is required to maintain a parse-time map of all defined type names in order
to know whether a less-than sign is a binary operator or a template
instantiation delimiter. That is, the following is ambiguous without context:

  foo < bar > x;

If C<foo> is a type, then we've said "define a variable C<x>"; otherwise, we've
said "invoke the (presumably overloaded) less-than operator on the value
C<foo>". I assume types take precedence over values, or maybe it's illegal to
have a single name bind to both.

Anyway, this is all complicated by the fact that type visibility is tied to
namespaces; so obvious strategies like "flag a set of identifiers" don't work.
It's simpler to define a pure-syntax CTTI to represent C++ type names, then
capture it as an erased value for parsing purposes. This also has other
advantages, one of them being that CTTIs can manage template instantiation and
provide the Turing-complete metaprogramming environment we all know and love.

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
