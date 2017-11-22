=head1 phi bootstrap parsers
Enough rewriting rules to have phi host its own semantics.
=cut

package phi::boot;

use strict;
use warnings;

use phi::parseapi ':all';
use phi::compiler ':all';


=head1 Literal parsers
These are installed on the base scope.
=cut

sub constant { phi::compiler::value->constant(@_) }

use constant string_literal  => string  >>sub { constant string  => $_[1] };
use constant integer_literal => integer >>sub { constant integer => $_[1] };
use constant unknown_literal => ident   >>sub { constant unknown => $_[1] };
use constant expr            => phi::compiler::scope_parser->new;


=head1 Base parse continuations
Enough to get methods and calls. Let's talk about how this works.

phi starts by parsing your source code as a string, forming a data structure in
the process. Part of this involves maintaining a scope to resolve literals and
variables. That scope, in turn, evaluates expressions as they're generated and
produces parse continuations as requested by values. This behavior is
customizable by using C<.#parse_continuation>.

Parse continuations are invoked with both the scope and the value as closure
arguments. Evaluation happens instantly.
=cut

use constant method_continuation =>
  method >>sub { phi::compiler::value->method($_[0]->[1], $_[1]) };

use constant call_continuation =>
  expr   >>sub { phi::compiler::value->call($_[0]->[1], $_[1]) };

use constant generic_continuation =>
  phi::compiler::value->hosted(
    parser => method_continuation | call_continuation);


=head2 Default continuations
Values support methods and function calls at parse time unless otherwise
specified with a custom continuation.
=cut

use constant default_continuations =>
  phi::compiler::match_method->new(phi::compiler::emit->new,
                                   '#parse_continuation')
  >>sub { generic_continuation };


=head1 Parentheses
We could theoretically write this in the language, but it's easy to provide
here.
=cut

use constant paren_syntax => expr->parens;


=head1 Turning values into parsers
We need a couple of base bindings in order to make this work.
=cut

sub type_predicate($)
{
  phi::compiler::match_method->new(
    phi::compiler::emit->new,
    '#type',
    phi::compiler::match_constant->new(string => shift));
}

use constant integer_predicate =>
  phi::compiler::value->hosted(parser => type_predicate('int'));

use constant string_predicate  =>
  phi::compiler::value->hosted(parser => type_predicate('string'));

use constant unknown_predicate =>
  phi::compiler::value->hosted(parser => type_predicate('unknown'));


=head1 Name bindings
We have to implement bindings in terms of unknown-rewriting rather than
literally (or at least, we often want to). If we don't, we'll start taking
chunks out of words that start with names we've bound -- which could be
particularly bad for things starting with C<e>, for instance. So we parse the
whole word and then decide.
=cut

sub name_binding($$)
{
  my ($name, $value) = @_;
  phi::compiler::match_constant->new(unknown => $name) >>sub { $value };
}


=head1 Bootstrap scope
This is a scope populated with literals and a small handful of bindings
sufficient to get the language going. The boot scope, like all scopes, is a
mixture of string and structural parsers.
=cut

use constant boot_scope => phi::compiler::scope->new(undef,
  phi::parser::alt_fixed->new(reverse map $_->spaced,
    unknown_literal,
    integer_literal,
    string_literal,
    paren_syntax,
    default_continuations,

    name_binding(int     => integer_predicate),
    name_binding(string  => string_predicate),
    name_binding(unknown => unknown_predicate)));


1;
