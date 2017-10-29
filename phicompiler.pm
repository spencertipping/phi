=head1 phi compiler
Before I get into how this works, I'll go through a simple example.

=head2 Example: the point struct
Let's write a function that takes two 2D vectors and returns the dot product.

  # this prints 14 (not 11; I am bad at math)
  print phi::c99->new->phi(q{
    # NB: it's fine (and necessary) for types to be mutable objects like this
    point = struct x:double y:double
    point 'dot point = |a b| ax*bx + ay*by
    point 1 2 dot point 3 4
  });

=head3 What's going on here
First, I need to mention that there are two runtimes involved here:

1. The hosting runtime, which is required to parse the phi code.
2. The target runtime (C99), which will end up running the code.

Let's start with the hosting runtime. It gets asked for an abstract evaluation
of the code; this builds up a graph of expressions, dependencies, and functions.
In the above example, the abstract evaluator will do all the work for us,
producing the value C<14> as the result. This is a result of constant-folding
and monomorphic function calls; in other words, the code above has no
dependencies on unknown values.

...so we're gonna generate a C file that hosts an RPC server whose only purpose
is to reply with C<14>. Not very interesting, but informative in terms of how
phi thinks about compiling things.

=head3 Abstract evaluation: more details
Going line by line:

  point struct x:double y:double

phi's parser begins by identifying an unidentified word, C<point>. From the
hosting runtime's point of view, "unidentified word" is a struct (making it a
meta-struct from the target's perspective) that supports operations like "what's
the next parser" and "produce an abstract value" (NB: unknown words can't
produce values).

"Unidentified word" returns a parser one of whose alternatives is C<"=" value>.
That parser then encounters the known word C<struct>, which itself is an
instance of what in the hosting runtime would be a meta-struct, and which
specifies a parser that consumes C<x:double y:double> and returns a struct
value. C<"=" value> then takes this and returns a statement-journal that
requests a binding from C<point> to this value.

The host runtime's parser then flatmaps, consuming this journal entry and
returning a parser that was the same as before but recognizes C<point> as the
value we bound to it. This transparency is implemented by the parser itself; the
runtime is unaware of any such variable binding.

Next line:

  point 'dot point = |a b| ax*bx + ay*by

After the flatmap, C<point> returns a value within the host runtime; this value
is asked for its continuation parser, one of whose alternatives is
C<op-binding>, which expands to a flatmap from a binding pattern to a function
that accepts typed arguments. (This type propagation needs to happen as a parser
flatmap because the function body can't be parsed unless we know the argument
types, and we don't want to impose on the user by having them repeat the type
names.)

Once we encounter C<|a b|>, we know the argument types; in fact, at this point
we expect a value of type C<function(double, double)> -- the continuation here
consumes C<|a b|> and flatmaps into a continuation that dispatches C<a> and C<b>
into abstract doubles. As before, the names are handled entirely by the parser;
the target runtime won't be aware that these values ever had names.

Within the function body we're at liberty to write C<ax> with no delimiters
_unless_ something else in scope is already called C<ax>. Longer names are
always preferred to shorter ones -- and C<ax> refers to the C<x> field of the
C<a> object only if it's parsed as a short name followed by a field spec (this
parsing is handled by the C<point> abstract being asked for its continuation).

C<*> is parsed by C<double>, which is the type of the abstract returned by
C<ax>. It then consumes C<*>, which is followed by a double-returning value
C<bx>, which in turn returns an abstract C<double>.

Technically we'd expect this strategy to produce strict left-to-right operator
precedence (i.e. left-associative with no precedence at all), which is exactly
what would happen if C<double> implemented a simple parser. But because C<bx> is
itself a double value, the initial arithmetic context continues until we have no
more binary operators left; then C<double> implements operator precedence.
Importantly, this means that different types can implement different precedence
strategies (and between types, precedence is such that semantics make sense).

This whole statement emits a new journal entry that invokes a meta-operation on
the abstract struct C<point>; the continuation of this journal contains a
rebinding of C<point> that specifies a new alternative for C<dot>. At this point
the target runtime is not committed to any code because no abstract value
depends on anything bound to the target.

Next line:

  point 1 2 dot point 3 4

This is parsed exactly as you'd expect: one of C<point>'s alternatives is a
constructor, which delegates to the two C<double>s to parse its arguments. So
C<point 1 2>, predictably, produces an abstract C<point> whose values happen to
be known -- i.e. bound in every runtime context. This abstract point then
returns a parser that uses the C<dot> alternative we defined above, which parses
the word C<dot> and the point value on the right-hand side. This points to a
function, C<|a b| ax*bx + ay*by>, which is applied at parse-time to the abstract
values. Here's how that works.

First let's remember that the last line journaled an alternative that mapped
C<dot> to a function. This is implemented by using a parser-map operation: the
function body consumes the input arguments of C<dot> and returns a new abstract
value. This means every function invocation happens inline (there's an exception
you can make if you need recursion).

That, in turn, means that C<point 1 2 dot point 3 4> is parse-equivalent to
C<1*2 + 3*4>, which, because every value is a constant, is parse-equivalent to
C<14>. This constant folding is implemented by abstract values, which will
evaluate as soon as they have no unknowns (that is, their C<op> operation is
overloaded to reflect their constant/variant state).
=cut

package phi::compiler;

use strict;
use warnings;

BEGIN
{
  require 'parser.pm';
  require 'parsestr.pm';
  require 'parseapi.pm';

  # HACK
  require 'render.pm';
}


=head1 Parser shorthands
Accessors for common parsing idioms.
=cut

sub oc { phi::parser::strclass->one_of(@_) }
sub mc { phi::parser::strclass->many_of(@_) }
sub Mc { phi::parser::strclass->more_of(@_) }
sub oe { phi::parser::strclass->one_except(@_) }
sub me { phi::parser::strclass->many_except(@_) }
sub Me { phi::parser::strclass->more_except(@_) }

sub at($) { eval "sub {shift->[$_[0]]}" }
sub as($)
{
  no strict 'refs';
  my ($class, @parents) = split /\s+/, shift;
  my %isa = map +($_ => 1), map "phi::compiler::$_", @parents;
  eval "sub ${_}::_{}" for keys %isa;
  @{"phi::compiler::${class}::ISA"} = ("phi::compiler::node", keys %isa);
  eval "#line 1 \"as(phi::compiler::$class)\"
        sub {bless { value  => \$_[0],
                     result => \$_[1] }, 'phi::compiler::$class'}";
}

sub str($) { phi::parser::strconst->new(shift) }
sub sd($)  { str(shift) >>as"delimiter rcolor rc6 v" }
sub cc($)  { shift->val->parse_continuation }

# NB: single-arg, but multi-prototype to modify precedence
sub mut(@) { phi::parser::mutable->new(shift) }
sub alt(@) { phi::parser::alt_fixed->new(shift) }


=head1 phi base syntax elements
Syntax primitives out of which we build other values.
=cut

use constant
{
  ident => oc('a'..'z', 'A'..'Z', '_')
         + mc('a'..'z', 'A'..'Z', '_', 0..9) >>as"ident vjoin",

  string => sd('"')
          + ((Me("\\\"")           >>as"str_chars  rcolor rc2 v"
              | str("\\") + oe('') >>as"str_escape rcolor rc5 vjoin") * 0
             >>as"str_content rall vjoin")
          + sd('"')
        >>as"string rall v1",

  whitespace => (  str('#') + me("\n") >>as"line_comment rcolor rc4 vjoin"
                 | Mc(" \n\r\t")       >>as"space        rcolor rc0 v") * 0
                >>as"whitespace rall"
};


=head1 phi language parser
This is deceptively simple. All we need to do is bootstrap a context that
defines the set of globals we can do anything with, like C<struct>, and that
resolves any unidentifiable word into that class.
=cut

sub expr($)
{
  my ($atom) = @_;
  my $circular = mut atom >\&cc;
  $circular->val = sd("(") + $circular + sd(")") >>as"parens rall v1"
                 | $circular;
  whitespace + $circular + whitespace >>as"expr rall v1";
}

use constant uword => ident >>as"uword rf vf";


=head1 phi meta-predefined values
Entry points for the language. We need a few of these:

1. Literal values (these form an entry point for parse expressions)
2. Predefined things like C<struct>
3. Unknown words
=cut

sub phi::compiler::uword::parse_continuation
{
  my ($self) = @_;
  sd("=") + expr    # HA HA HA no fucking way

  # Ok here's the problem. Let's suppose we have a new entry point for the
  # parser; when this function is constructing the parse continuation, how does
  # it know about it? Do we pass the prior value of `expr` into here?
  #
  # I think we need a "world" parser that somehow gets threaded through.
}


1;
