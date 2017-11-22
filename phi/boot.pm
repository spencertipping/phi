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
sub hosted   { phi::compiler::value->hosted(@_) }

use constant string_literal  => string  >>sub { constant string  => $_[1] };
use constant integer_literal => integer >>sub { constant int     => $_[1] };
use constant unknown_literal => ident   >>sub { constant unknown => $_[1] };
use constant expr            => phi::compiler::scope_parser->new;


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
  hosted parser => method_continuation | call_continuation;


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
  phi::compiler::match_rewritten->new(
    phi::compiler::emit->new,
    '#type',
    phi::compiler::match_constant->new(string => shift));
}

sub op_predicate($)
{
  phi::compiler::match_rewritten->new(
    phi::compiler::emit->new,
    '#op',
    phi::compiler::match_constant->new(string => shift));
}

sub arg_predicate($)
{
  phi::compiler::match_arg->new(shift);
}

use constant integer_predicate => hosted parser => type_predicate 'int';
use constant string_predicate  => hosted parser => type_predicate 'string';
use constant unknown_predicate => hosted parser => type_predicate 'unknown';

use constant op_resolver =>
  phi::compiler::match_method->new(
    phi::compiler::emit->new,
    '#op')
  >>sub { constant string => $_[1]->{op} };

use constant type_resolver =>
  phi::compiler::match_method->new(
    op_predicate('constant') | op_predicate('hosted') | op_predicate('arg'),
    '#type')
  >>sub { constant string => $_[1]->{type} };

use constant int_arg_resolver =>
  phi::compiler::match_method->new(type_predicate('int'), '#arg')
  >>sub { phi::compiler::value->arg(any => $_[1]->get('int')) };

use constant argn_resolver =>
  phi::compiler::match_method->new(op_predicate('arg'), '#arg_n')
  >>sub { constant int => $_[1]->{val} };


=head2 The C<#as_parser> method
This essentially quotes things, emitting unknowns and descending into function
calls.
=cut

use constant string_as_parser =>
  phi::compiler::match_method->new(type_predicate('string'), '#as_parser')
  >>sub { hosted parser => phi::compiler::match_constant->new(
                             string => $_[1]->{val}) };

use constant int_as_parser =>
  phi::compiler::match_method->new(type_predicate('int'), '#as_parser')
  >>sub { hosted parser => phi::compiler::match_constant->new(
                             int => $_[1]->{val}) };

use constant arg_as_parser =>
  phi::compiler::match_method->new(op_predicate('arg'), '#as_parser')
  >>sub { my ($context, $arg) = @_;
          my ($scope) = @$context;
          $scope->method(constant(string => "phi doesn't support variable shadowing"),
                         '#compile_error') };

use constant unknown_as_parser =>
  phi::compiler::match_method->new(type_predicate('unknown'), '#as_parser')
  >>sub { hosted parser => phi::compiler::emit->new };

use constant method_as_parser =>
  phi::compiler::match_method->new(op_predicate('method'), '#as_parser')
  >>sub { my ($context, $method) = @_;
          my ($scope) = @$context;
          hosted parser =>
            phi::compiler::match_method->new(
              $scope->method($$method{val}, '#as_parser')->get('parser'),
              $$method{method}) };

use constant call_as_parser =>
  phi::compiler::match_method->new(op_predicate('call'), '#as_parser')
  >>sub { my ($context, $call) = @_;
          my ($scope) = @$context;
          hosted parser =>
            phi::compiler::match_call->new(
              $scope->method($$call{val}, '#as_parser')->get('parser'),
              $scope->method($$call{arg}, '#as_parser')->get('parser')) };


=head2 Collecting unknowns
This is used internally to bind unknowns to their corresponding values within
the body of a function/replacement.
=cut

use constant string_unknowns =>
  phi::compiler::match_method->new(type_predicate('string'), '#unknowns')
  >>sub { hosted array => [] };

use constant int_unknowns =>
  phi::compiler::match_method->new(type_predicate('int'), '#unknowns')
  >>sub { hosted array => [] };

use constant unknown_unknowns =>
  phi::compiler::match_method->new(type_predicate('unknown'), '#unknowns')
  >>sub { hosted array => [$_[1]] };

use constant method_unknowns =>
  phi::compiler::match_method->new(op_predicate('method'), '#unknowns')
  >>sub { my ($context, $m) = @_;
          my ($scope) = @$context;
          $scope->method($$m{val}, '#unknowns') };

use constant call_unknowns =>
  phi::compiler::match_method->new(op_predicate('call'), '#unknowns')
  >>sub { my ($context, $c) = @_;
          my ($scope) = @$context;
          hosted array =>
            [@{$scope->method($$c{val}, '#unknowns')->get('array')},
             @{$scope->method($$c{arg}, '#unknowns')->get('array')}] };


=head2 Destructured scopes
Now we have enough to create a child scope to bind parse outputs to variables.
=cut

use constant parser_scope_continuation =>
  phi::compiler::match_method->new(phi::compiler::emit->new,
                                   '#scope_continuation')
  >>sub { my ($context, $p) = @_;
          my ($scope) = @$context;

          # Generate variable bindings by collecting unknowns.
          my $vars = $scope->method($p, '#unknowns')->get('array');
          my @var_alt;

          die "expected array ref, got " . ref($vars)
            unless ref($vars) eq 'ARRAY';

          # FIXME: is it possible to have nested definitions?
          for my $i (0..$#$vars)
          {
            push @var_alt, name_binding($$vars[$i]->get('unknown') =>
                                        phi::compiler::value->arg(any => $i));
          }

          hosted scope => phi::compiler::scope->new($scope,
                            phi::parser::alt_fixed->new(@var_alt)) };


use constant parser_bind =>
  phi::compiler::match_method->new(
    phi::compiler::match_method->new(phi::compiler::emit->new, '#equals'),
    '#scope_continuation')
  >>sub { my ($context, $expr) = @_;
          my ($scope) = @$context;
          my $parser = $scope->method($expr, '#as_parser');
          my $child = $scope->method($expr, '#scope_continuation')->get('scope');
          hosted parser =>
            $child + str(';')->syntax
            >>sub { $scope->call($scope->method($parser, 'map_to'), $_[1]) } };


use constant binding_continuation =>
  phi::compiler::match_method->new(
    phi::compiler::match_call->new(
      phi::compiler::match_method->new(type_predicate('parser'), 'map_to'),
      phi::compiler::emit->new),
    '#parse_continuation')
  >>sub
    {
      my ($context, $p, $v) = @_;
      my ($scope) = @$context;
      hosted parser =>
        $scope
        | $p->get('parser')
          >>sub
            {
              my ($inner_context, @emitted) = @_;
              my @rewrites;
              for my $i (0..$#emitted)
              {
                push @rewrites, arg_predicate($i) >>sub { $emitted[$i] };
              }

              $scope->call($scope->method($v, '#rewrite_with'),
                           hosted scope =>
                             phi::compiler::scope->new($scope,
                               phi::parser::alt_fixed->new(@rewrites)));
            } };


=head2 Term rewriting
Hosted in phi, naturally. This way you can specify new node types or track
metadata.
=cut

use constant string_rewrite_with =>
  phi::compiler::match_call->new(
    phi::compiler::match_method->new(type_predicate('string'), '#rewrite_with'),
    phi::compiler::emit->new)
  >>sub { $_[1] };

use constant int_rewrite_with =>
  phi::compiler::match_call->new(
    phi::compiler::match_method->new(type_predicate('int'), '#rewrite_with'),
    phi::compiler::emit->new)
  >>sub { $_[1] };

use constant unknown_rewrite_with =>
  phi::compiler::match_call->new(
    phi::compiler::match_method->new(type_predicate('unknown'), '#rewrite_with'),
    phi::compiler::emit->new)
  >>sub { my ($context, $unknown, $scope) = @_;
          $scope->get('scope')->simplify($unknown) };

use constant arg_rewrite_with =>
  phi::compiler::match_call->new(
    phi::compiler::match_method->new(op_predicate('arg'), '#rewrite_with'),
    phi::compiler::emit->new)
  >>sub { my ($context, $arg, $scope) = @_;
          $scope->get('scope')->simplify($arg) };

use constant method_rewrite_with =>
  phi::compiler::match_call->new(
    phi::compiler::match_method->new(op_predicate('method'), '#rewrite_with'),
    phi::compiler::emit->new)
  >>sub { my ($context, $m, $hosted_scope) = @_;
          my $scope = $hosted_scope->get('scope');
          $scope->method($scope->call(
                           $scope->method($$m{val}, '#rewrite_with'),
                           $hosted_scope),
                         $$m{method}) };

use constant call_rewrite_with =>
  phi::compiler::match_call->new(
    phi::compiler::match_method->new(op_predicate('call'), '#rewrite_with'),
    phi::compiler::emit->new)
  >>sub { my ($context, $c, $hosted_scope) = @_;
          my $scope = $hosted_scope->get('scope');
          $scope->call($scope->call(
                         $scope->method($$c{val}, '#rewrite_with'),
                         $hosted_scope),
                       $scope->call(
                         $scope->method($$c{arg}, '#rewrite_with'),
                         $hosted_scope)) };


=head2 Parser application
Hosted parsers can be applied to values to produce outputs, which in this case
are hosted arrays (TODO: fix this).
=cut

use constant parser_apply =>
  phi::compiler::match_call->new(type_predicate('parser'),
                                 phi::compiler::emit->new)
  >>sub { my ($context, $parser, $val) = @_;
          my ($scope) = @$context;
          my ($ok, $l, $x) = $parser->get->parse($val, 0, $scope);
          $ok ? $scope->call(
                  $scope->method(constant(unknown => 'parse_result'), 'success'),
                  $x)
              : $scope->method(constant(unknown => 'parse_result'), 'fail') };


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

    op_resolver,
    type_resolver,
    argn_resolver,
    int_arg_resolver,

    unknown_as_parser,
    method_as_parser,
    call_as_parser,
    string_as_parser,
    int_as_parser,
    arg_as_parser,

    unknown_unknowns,
    method_unknowns,
    call_unknowns,
    string_unknowns,
    int_unknowns,

    unknown_rewrite_with,
    arg_rewrite_with,
    method_rewrite_with,
    call_rewrite_with,
    string_rewrite_with,
    int_rewrite_with,

    parser_scope_continuation,
    parser_bind,
    parser_apply,

    binding_continuation,

    name_binding(int     => integer_predicate),
    name_binding(string  => string_predicate),
    name_binding(unknown => unknown_predicate)));


1;
