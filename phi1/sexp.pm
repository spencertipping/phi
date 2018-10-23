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

use constant ops_as_fns    => {};
use constant special_forms => {};

package phi::sexp_list
{
  use overload qw/ "" str @{} xs /;

  sub new
  {
    my ($class, @xs) = @_;
    bless { xs         => \@xs,
            frame_slot => undef }, $class;
  }

  sub str  { "(" . join(" ", @{+shift}) . ")" }
  sub xs   { shift->{xs} }
  sub head { shift->xs->[0] }

  sub compile
  {
    # NB: this function binds a linear frame variable for the result of this
    # sexp and returns the name of that variable.
    my ($self, $frame, $asm) = @_;
    my ($h, @t) = @{$self->xs};
    return phi::special_forms->{$h}->($frame, $asm, @t)
      if exists phi::special_forms->{$h};

    # If we aren't a special form, then we're either a method call or a regular
    # function. Either of those cases involves allocating a temporary for each
    # argument, then stacking them and calling the function (or method).
    #
    # Some ops stand in for functions and are basically inlines.
    return $self->compile_method($frame, $asm) if $h =~ /^\./;
    return $self->compile_op($frame, $asm)     if exists phi::ops_as_fns->{$h};
    return $self->compile_fncall($frame, $asm);
  }

  sub compile_method
  {
    my ($self, $frame, $asm) = @_;
    my ($h, @t) = @{$self->xs};

    # @t contains ($receiver, $arg1, ..., $argn), but in their pre-compiled
    # forms. Compile each one to get its frame binding (which will probably be
    # linear), then emit accessors to push them onto the stack in reverse and
    # call the method against the receiver.
    my ($receiver, @args) = map $_->compile($frame, $asm), @t;
    $frame->get($asm, $_) for reverse $receiver, @args;
    my $ctti = $frame->ctti($receiver);

      $ctti eq 'ptr'     ? $asm->mb(substr $h, 1)
    : $ctti eq 'hereptr' ? $asm->mh(substr $h, 1)
    : die "compile_method: ($h @t) on a receiver whose CTTI is $ctti";

    # Now bind the return value. A polymorphic method call doesn't indicate its
    # return CTTI, so we need to treat the result as an int and wait for the
    # user to coerce or bind it.
    my $return = gensym;
    $frame->bind($return, 'int', 1)
          ->set($asm, $return);
    $return;
  }

  sub compile_op
  {
    my ($self, $frame, $asm) = @_;

  }
}


=head2 Special forms
=cut


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
