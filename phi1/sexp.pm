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

  (int <name> <value>)                  # also works as unscoped (let)
  (ptr <name> <value>)
  (hereptr <name> <value>)
  (fn (<type> <name> ...) <body...>)
  (do <stuff...>)                       # (do) == (progn)
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
use constant defined_fns   => {};

package phi::sexp_var
{
  use overload qw/ "" str /;
  sub new     { bless \(my $v = $_[1]), $_[0] }
  sub compile { ${+shift} }
  sub str     { ${+shift} }
}

package phi::sexp_const
{
  use overload qw/ "" str /;
  sub new
  {
    my ($class, $ctti, $v) = @_;
    bless { ctti => $ctti, value => $v }, $class;
  }

  sub str
  {
    my ($self) = @_;
    sprintf "%x:%s", $$self{value}, $$self{ctti};
  }

  sub compile
  {
    my ($self, $frame, $asm) = @_;
    my $gensym = phi::gensym;
    $asm->l($$self{value});
    $frame->bind($gensym, $$self{ctti}, 1)
          ->set($asm, $gensym);
    $gensym;
  }
}

package phi::sexp_list
{
  use overload qw/ "" str @{} xs /;

  sub new
  {
    my ($class, @xs) = @_;
    bless { xs => \@xs }, $class;
  }

  sub str  { "(" . join(" ", @{+shift}) . ")" }
  sub xs   { shift->{xs} }
  sub head { shift->xs->[0] }

  sub bind_return
  {
    my ($self, $frame, $asm, $ctti) = @_;
    my $gensym = phi::gensym;
    $frame->bind($gensym, $ctti, 1)
          ->set($asm, $gensym);
    $gensym;
  }

  sub compile
  {
    # NB: this function binds a linear frame variable for the result of this
    # sexp and returns the name of that variable.
    my ($self, $frame, $asm) = @_;
    my ($h, @t) = @$self;

    return phi::special_forms->{$h}->($frame, $asm, @t)
      if exists phi::special_forms->{$h};

    # If we aren't a special form, then we're either a method call or a regular
    # function. Either of those cases involves allocating a temporary for each
    # argument, then stacking them and calling the function (or method).
    #
    # Some ops stand in for functions and are basically inlines.
    return $self->compile_method($frame, $asm) if $h =~ /^\./;
    return $self->compile_op($frame, $asm)     if exists phi::ops_as_fns->{$h};
    return $self->compile_fncall($frame, $asm) if exists phi::defined_fns->{$h};
    die "sexp.compile: not sure what to do with $self";
  }

  sub compile_method
  {
    my ($self, $frame, $asm) = @_;
    my ($h, @t) = @$self;

    # @t contains ($receiver, $arg1, ..., $argn), but in their pre-compiled
    # forms. Compile each one to get its frame binding (which will probably be
    # linear), then emit accessors to push them onto the stack in reverse and
    # call the method against the receiver.
    my ($receiver, @args) = map $_->compile($frame, $asm), @t;
    my $ctti = $frame->ctti($receiver);

    $frame->get($asm, $_) for reverse $receiver, @args;
      $ctti eq 'ptr'     ? $asm->mb(substr $h, 1)
    : $ctti eq 'hereptr' ? $asm->mh(substr $h, 1)
    : die "compile_method: ($h @t) on a receiver whose CTTI is $ctti";

    # Now bind the return value. A polymorphic method call doesn't indicate its
    # return CTTI, so we need to treat the result as an int and wait for the
    # user to coerce or bind it.
    $self->bind_return($frame, $asm, 'int');
  }

  sub compile_op
  {
    my ($self, $frame, $asm) = @_;
    my ($h, @t) = @$self;

    # Same as compile_method, but issue a bytecode op directly.
    $frame->get($asm, $_) for reverse map $_->compile($frame, $asm), @t;
    $asm->C($phi::bytecodes{$h});
    $self->bind_return($frame, $asm, phi::ops_as_fns->{$h}->{ctti});
  }

  sub compile_fn
  {
    my ($self, $frame, $asm) = @_;

    # Unlike compile_method and compile_op, the function itself is subject to
    # evaluation here -- so evaluate every element of this sexp in order, then
    # push them in reverse followed by a single call instruction.
    #
    # This means the function must be a hereptr.
    $frame->get($asm, $_) for reverse map $_->compile($frame, $asm), @$self;
    $asm->call;
    $self->bind_return($frame, $asm, phi::defined_fns->{$self->head}->{ctti});
  }
}


=head2 Special forms
Delegation functions for special form compilation. Each of these needs to
produce a single value referenced to the enclosing frame, which means that the
toplevel will end up getting its own frame class.
=cut

sub defspecial($$) { special_forms->{$_[0]} = $_[1] }

for my $ctti (qw/ int ptr hereptr /)
{
  defspecial $ctti => sub
  {
    my ($frame, $asm, $name, $vsexp) = @_;
    my $value = $vsexp->compile($frame, $asm);
    $frame->bind($name, $ctti)
          ->get($asm, $value)           # consume the linear
          ->set($asm, $name);
    $name;
  };
}

defspecial do => sub
{
  # Fairly normal strict evaluation; just throw away each intermediate value we
  # produce. We need to make a get() to each one to dispose of linears.
  my ($frame, $asm, @xs) = @_;
  my $last = pop @xs;
  $frame->get($asm, $_->compile($frame, $asm)), $asm->drop for @xs;
  $last->compile($frame, $asm);
};

defspecial fn => sub
{
  my ($frame, $asm, $args, @body) = @_;

  # TODO: we have a problem. $frame->enter requires that we know the full set of
  # locals, which means sexps need to be able to describe that set before
  # compiling anything.
  my $fn_frame = phi::frame->new;
  my @args = @$args;
  while (@args)
  {
    my $ctti = shift @args;
    my $name = shift @args;
    $fn_frame->bind($name, $ctti);
  }

};


=head2 Parsing
Keeping things simple: let's advance using regular expressions and maintain
parse position with C<pos()>.
=cut

sub read_space() { /\G(?:\s+|#.*\n?)*/gc }

sub read_sexp_();
sub read_sexp_list()
{
  my @elements;
  push(@elements, read_sexp_), read_space until /\G\)/gc && read_space;
  phi::sexp_list->new(@elements);
}

sub read_sexp_atom()
{
    /\G(\d+)/gc   ? phi::sexp_const->new(int => 0 + $1)
  : /\G\.(\w+)/gc ? ".$1"
  : /\G(\w+)/gc   ? phi::sexp_var->new($1)
  : die "unknown atom starting at " . substr $_, pos;
}

sub read_sexp_()
{
  return read_sexp_list if /\G\(/gc && read_space;
  return read_sexp_atom;
}

sub read_sexp($)
{
  local $_ = shift;
  my @r;
  pos($_) = 0;
  read_space;
  push(@r, read_sexp_), read_space while length > pos;
  wantarray ? @r : $r[0];
}


1;
