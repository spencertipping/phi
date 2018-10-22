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
  (fn (<type> <name> ...) <body...>)
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
  use overload qw/ "" str @{} xs /;

  sub new
  {
    my ($class, @xs) = @_;
    bless { xs         => \@xs,
            frame_slot => undef }, $class;
  }

  sub str { "(" . join(" ", @{+shift}) . ")" }
  sub xs  { shift->{xs} }

  sub compile
  {
    my ($self, $frame, $asm) = @_;

  }
}


=head2 Parsing
Keeping things simple: let's advance using regular expressions.
=cut

sub read_whitespace() { /\G(?:\s+|#.*\n?)*/gc }

sub read_sexp_();
sub read_sexp_list()
{
  my @elements;
  read_whitespace;
  until (/\G\)/gc)
  {
    push @elements, read_sexp_;
    read_whitespace;
  }
  phi::sexp_list->new(@elements);
}

sub read_sexp_atom()
{
    /\G(\d+)/gc   ? 0 + $1
  : /\G\.(\w+)/gc ? ".$1"
  : /\G(\w+)/gc   ? "$1"
  : die "unknown atom starting at " . substr $_, pos;
}

sub read_sexp_()
{
  read_whitespace;
  return read_sexp_list if /\G\(/gc;
  return read_sexp_atom;
}

sub read_sexp($)
{
  local $_ = shift;
  pos($_) = 0;
  read_sexp_;
}


1;
