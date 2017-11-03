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

  point = struct x:double y:double

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
requests a binding from C<point> to this value. Technically, this is done by
returning an abstract value that, upon request, links a new definition into the
enclosing scope.

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

=head2 Relationship between parsing and abstract values
phi provides a strong tie between abstract values and parsed quantities -- and
not just because abstract values parse the code. The coupling also makes it
possible for language editors to provide introspection and features like
autocomplete. In other words, although in theory the language exists
independently from both text and editors, in practice it's tightly coupled to
both.

=head3 ...the tradeoff
The moment we introduce this dependency, of course, we're demanding that
abstract values understand something about the syntax of the language: they
supply the parsers that create constants and operator invocations.

This isn't a free dependency. If abstract values can write arbitrary parsing
rules, then they must also specify how the editor treats the results. This means
type authors, if they're doing anything custom, are specifying both a
representation+semantics, and a UI, for their type.

Now of course to some extent every language demands this of type authors; the
difference is that the language itself provides the UI elements to make it
happen. General UI element handling is pushed into the editor. So it isn't like
most languages don't have this problem; they just delegate the problem.

=head3 ...the implementation
phi can avoid the worst parts of this tradeoff (i.e. full syntactic anarchy
thrown at type authors) by providing libraries of parsers and parsing patterns.
If type authors need whole new classes of literals, or something similar
complicated, they have the ability to write those and provide editor support.
It's fine for us to assume we're working in a text-based world; the UI concept
doesn't need to generalize to HTML/canvas/graphical stuff at this point.

The standard base syntactic elements are defined in phi::syntax.
=cut

package phi::compiler;

use strict;
use warnings;

use phi::parseapi 'mut';
use phi::syntax ':all';


=head1 Core language elements
Management structures to parse basic things like sequences of statements with
local variable scoping.
=cut

package phi::compiler::scope
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $parent, $previous, @defs) = @_;
    bless { parent   => $parent,
            previous => $previous,
            defs     => \@defs,
            atom     => phi::parser::alt_fixed->new(
                          @defs,
                          defined $previous ? $previous : (),
                          defined $parent   ? $parent   : ()) }, $class;
  }

  sub parse
  {
    my ($self, $input, $start) = @_;
    $$self{atom}->parse($input, $start);
  }

  sub bind
  {
    my $self = shift;
    ref($self)->new($$self{parent}, $self, @_);
  }

  sub previous { shift->{previous} }
  sub parent   { shift->{parent} }
}


package phi::compiler::block
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $scope) = @_;
    bless { initial_scope => $scope }, $class;
  }

  sub parse
  {
    my ($self, $input, $start) = @_;
    my $offset = $start;
    my $scope  = $$self{initial_scope};
    my @xs;
    for (my ($ok, $l, $r);
         ($ok, $l, $r) = $scope->parse($input, $offset) and $ok;
         $offset += $l)
    {
      push @xs, $r;
      $scope = $r->scope_continuation($scope);
    }
    $self->return($offset - $start, @xs);
  }
}


# TODO
# This won't quite work because we could have an operator that returns a value
# of a different type in the middle. This needs to work incrementally, perhaps
# by using linkages, and search for a solution that works.
#
# Alternatively, we can just use the semantics that until you jump into parens
# or break the context, you get the precedence specified by the atom you're
# working with.
package phi::compiler::precedence_op_parser
{
  use parent -norequire => 'phi::parser::parser_base';

  sub new
  {
    my ($class, $v, $precedence, %op_continuations) = @_;
    my %p;
    for my $i (0..$#$precedence)
    { $p{$_} = $i for split /\s+/, $$precedence[$i] }

    my $alt = phi::parser::alt_fixed->new(
      map phi::parser::seq_fixed->new(
            phi::parser::strconst->new($_),
            $op_continuations{$_}),
          map split(/\s+/), @$precedence);

    bless { value      => $v,
            precedence => \%p,
            alt        => $alt }, $class;
  }

  sub parse
  {
    my ($self, $input, $start) = @_;
    my $offset = $start;
    my @ops;
    my @vs = ($$self{value});

    for (my ($ok, $l, $op, $v);
         ($ok, $l, $op, $v) = $$self{alt}->parse($start, $offset) and $ok;
         $offset += $l)
    {
      push @vs, $v;
      push @ops, $op;
      while (@ops >= 2 and $$self{precedence}{$ops[-1]}
                        <= $$self{precedence}{$ops[-2]})
      {
        my $op  = pop @ops;
        my $rhs = pop @vs;
        $vs[-1] = $vs[-1]->op($op, $rhs);
      }
    }

    $self->return($offset - $start, $vs[-1]);
  }
}


1;
