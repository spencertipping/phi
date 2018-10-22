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
use bytes;


=head1 S-expressions
For compilation purposes, lists dispatch on their first element. There are some
special forms:

  (def <name> <value>)
  (fn ((<type> <name>) ...) <body...>)
  (do <stuff...>)
  (let (<type> <name> <value>) <body...>)
  (if <cond> <then> <else>)
  (while <cond> <body...>)
  (return <value>)
  (= <name> <value>)
  (.<method> <receiver> <args...>)
  (<op> <args...>)

C<op> can be the name of any bytecode operation; if you use this form, it will
be inserted directly after arg processing. phi knows the arity of every
operator; nullary ops like C<mcpy> and C<mset> will return C<int(0)>, and
C<idiv> returns just the quotient.
=cut

package phi::sexp_list
{
  use overload qw/ "" str /;

  sub new
  {
    my ($class, @xs) = @_;
    bless \@xs, $class;
  }

  sub str { "(" . join(" ", @{+shift}) . ")" }

  # TODO: process-as-arg
  # TODO: compilation protocol, specifically frame class propagation
}


=head2 Parsing
Keeping things simple: let's advance using regular expressions.
=cut

sub read_whitespace($) { shift =~ /\G(?:\s+|#.*\n?)*/gc }

sub read_sexp_list($)
{
  my @elements;
  read_whitespace $_[0];
  until ($_[0] =~ /\G\)/gc)
  {
    push @elements, read_sexp_($_[0]);
    read_whitespace $_[0];
  }
  phi::sexp_list->new(@elements);
}

sub read_sexp_atom($)
{
    $_[0] =~ /\G(\d+)/gc   ? 0 + $1
  : $_[0] =~ /\G\.(\w+)/gc ? ".$1"
  : $_[0] =~ /\G(\w+)/gc   ? "$1"
  : die "unknown atom starting at " . substr $_[0], pos($_[0]);
}

sub read_sexp_($)
{
  read_whitespace $_[0];
  return read_sexp_list $_[0] if $_[0] =~ /\G\(/gc;
  return read_sexp_atom $_[0];
}

sub read_sexp($)
{
  pos($_[0]) = 0;
  goto &read_sexp_;
}


1;
