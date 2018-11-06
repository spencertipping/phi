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

  (def <name> <ctti> <value>)           # define within current frame
  (fn (<type> <name> ...) <body...>)
  (do <stuff...>)                       # (do) == (progn)
  (if <cond> <then> <else>)
  (while <cond> <body...>)
  (= <name> <value>)

There are also not-quite-special forms that are nonetheless compiled differently
from normal function calls:

  (.<method> <receiver> <args...>)      # a macro for polymorphic calls
  (<inline> <args...>)                  # used for bytecode op passthroughs

...and finally, the function call syntax we all know and love:

  (<fn-expr> <args...>)

=cut

use constant inlines       => {};
use constant special_forms => {};


=head2 S-expression data structures
Just enough variants to implement the right compilation logic, which consists of
two distinct phases: C<collect_bindings> to figure out how big the frame needs
to be, and C<compile> to write code into an asm object.
=cut

package phi::sexp_var
{
  use overload qw/ "" str /;
  sub new              { bless \(my $v = $_[1]), $_[0] }
  sub compile          { ${+shift} }
  sub str              { ${+shift} }
  sub collect_bindings { $_[1] }
}

package phi::sexp_const
{
  use overload qw/ "" str /;
  sub new
  {
    my ($class, $ctti, $v) = @_;
    bless { ctti  => $ctti,
            value => $v,
            var   => phi::gensym }, $class;
  }

  sub str
  {
    my ($self) = @_;
    sprintf "%x:%s", $$self{value}, $$self{ctti};
  }

  sub compile
  {
    my ($self, $frame, $asm) = @_;
    $frame->bind($$self{var}, $$self{ctti}, 1)
          ->set($asm->l($$self{value}), $$self{var});
    $$self{var};
  }

  sub collect_bindings
  {
    my ($self, $frame) = @_;
    $frame->bind($$self{var}, $$self{ctti});
  }
}

package phi::sexp_list
{
  use overload qw/ "" str @{} array /;

  sub new
  {
    my ($class, $h, @t) = @_;
    bless { tail => \@t,
            head => $h,
            var  => undef,
            ctti => undef }, $class;
  }

  sub str   { "(" . join(" ", @{+shift}) . ")" }
  sub array { [$_[0]->head, $_[0]->tail] }
  sub tail  { @{shift->{tail}} }
  sub head  { shift->{head} }
  sub var   { shift->{var} }

  sub collect_bindings
  {
    my ($self, $frame) = @_;
    my $h = $self->head;
    return phi::special_forms->{$h}->{collect_bindings}->($frame, $self)
      if exists phi::special_forms->{$h};

    $_->collect_bindings($frame) for $self->tail;
    $self->head->collect_bindings($frame)
      unless $h =~ /^\./ || exists phi::inlines->{$h};
  }

  sub compile
  {
    # NB: this function binds a linear frame variable for the result of this
    # sexp and returns the name of that variable.
    my ($self, $frame, $asm) = @_;
    my $h = $self->head;

    return phi::special_forms->{$h}->($frame, $asm, $self)
      if exists phi::special_forms->{$h};

    # If we aren't a special form, then we're either a method call or a regular
    # function. Either of those cases involves allocating a temporary for each
    # argument, then stacking them and calling the function (or method).
    #
    # Some ops stand in for functions and are basically inlines.
    return $self->compile_method($frame, $asm) if $h =~ /^\./;
    return $self->compile_op($frame, $asm)     if exists phi::inlines->{$h};
    $self->compile_fncall($frame, $asm);
  }

  sub compile_method
  {
    my ($self, $frame, $asm) = @_;

    # @t contains ($receiver, $arg1, ..., $argn), but in their pre-compiled
    # forms. Compile each one to get its frame binding (which will probably be
    # linear), then emit accessors to push them onto the stack in reverse and
    # call the method against the receiver.
    my $h = $self->head;
    my ($receiver, @args) = map $_->compile($frame, $asm), $self->tail;
    my $ctti = $frame->ctti($receiver);

    $frame->get($asm, $_) for reverse $receiver, @args;

    # Coerce ints to baseptrs if we call methods on them. If you want a hereptr,
    # you'll need to cast it.
    $ctti eq 'hereptr' ? $asm->mh(substr $h, 1)
                       : $asm->mb(substr $h, 1);

    # Now bind the return value. A polymorphic method call doesn't indicate its
    # return CTTI, so we need to treat the result as an int and wait for the
    # user to coerce or bind it.
    $frame->set($asm, $$self{var});
    $$self{var};
  }

  sub compile_op
  {
    my ($self, $frame, $asm) = @_;

    # Same as compile_method, but issue a bytecode op directly.
    $frame->get($asm, $_)
      for reverse map $_->compile($frame, $asm), $self->tail;
    phi::inlines->{$self->head}->($asm);
    $frame->set($asm, $$self{var});
    $$self{var};
  }

  sub compile_fncall
  {
    my ($self, $frame, $asm) = @_;

    # Unlike compile_method and compile_op, the function itself is subject to
    # evaluation here -- so evaluate every element of this sexp in order, then
    # push them in reverse followed by a single call instruction.
    #
    # This means the function must be a hereptr.
    $frame->get($asm, $_) for reverse map $_->compile($frame, $asm), @$self;
    $asm->call;
    $frame->set($asm, $$self{var});
    $$self{var};
  }
}


=head2 Special forms
Delegation functions for special form compilation. Each of these needs to
produce a single value referenced to the enclosing frame, which means that the
toplevel will end up getting its own frame class. This means we can't easily
define a REPL or (eval) since frames aren't extensible. phi2 sorts this out by
defining frame metaclasses that use open-ended storage for locals.
=cut

sub defspecial { special_forms->{$_[0]} = { @_[1..$#_] } }


=head3 C<(def name ctti val)>
Basically just alias a name to whatever gensym we already have for C<val>, and
make it a pinned, not linear, quantity.
=cut

defspecial 'def',
  collect_bindings => sub
  {
    my ($frame, $sexp) = @_;
    my ($name, $ctti) = $sexp->tail;
    $frame->bind($name->str, $ctti->str);
  },
  compile => sub
  {
    my ($frame, $asm, $sexp) = @_;
    my ($name, $ctti, $vexp) = $sexp->tail;
    $frame->get($asm, $vexp->compile($frame, $asm))
          ->set($asm, $name);
    $name;
  };


=head3 C<(do stuff...)>
Compile everything inline, forcing each linear by using C<< $frame->get >> and
then dropping the result. The last result is neither forced nor dropped.
=cut

defspecial 'do',
  collect_bindings => sub
  {
    my ($frame, $sexp) = @_;
    $_->collect_bindings($frame) for $sexp->tail;
  },
  compile => sub
  {
    # Fairly normal strict evaluation; just throw away each intermediate value
    # we produce. We need to make a get() to each one to dispose of linears.
    my ($frame, $asm, $sexp) = @_;
    my @xs   = @$sexp;
    my $last = pop @xs;
    $frame->get($asm, $_->compile($frame, $asm)), $asm->drop for @xs;
    $last->compile($frame, $asm);
  };


=head3 C<(fn (ctti arg ctti arg ...) body...)>
NB: totally a lexical closure using frame chaining. We need this in order to be
able to see globals from inside a function. Note, however, that these closures,
like all functions, use stack-allocated frames; this means you can _use_ a
closure, but you can't _return_ one. phi2 fixes this.

Functions are inlined using C<code()> and effectively managed as constants.
They're stored as nonlinear hereptrs because inline-allocation pins their memory
either way, so we might as well save the overhead of clearing the pointer on
every access. (I'm assuming access is more common than GC.)

Each function receives its lexical parent frame as an argument immediately
beneath the continuation, but that lexical parent is set up at compile-time.
This means we need code that writes the parent frame address into a code segment
like this:

  fn_var = code(fn)
  code(l(parent_frame) l(fn_var) call)

The only catch is that we can't use C<code> for the closure allocation because
C<code> is shared across instances of the same frame. We need all of the closure
data to belong to whichever frame created it.

TODO: figure this out
=cut

defspecial 'fn',
  collect_bindings => sub
  {
    my ($frame, $sexp) = @_;
    $frame->bind($$sexp{var} //= gensym,
                 $$sexp{ctti} = 'hereptr');
  },
  compile => sub
  {
    my ($frame, $asm, $sexp) = @_;
    my ($args, @body) = $sexp->tail;
    my $fn_frame = $frame->child;
    my %argtypes = reverse @$args;
    $fn_frame->bind($_ => $argtypes{$_}) for keys %argtypes;
    $_->collect_bindings($fn_frame) for @body;

    # Now compile the function body into a separate assembler and then inline it
    # into the parent asm using code(). We can't allocate it separately because
    # we don't currently have logic to trace the reference set for a code block.
    my $fn_asm = phi::asm->new;

    # FIXME: push lexical parent frame somewhere

    # We can now safely compile a frame-enter. The initial stack layout will be
    # argN ... arg3 arg2 arg1 cc, and the frame-enter will pop cc. It's up to us
    # to populate the other args.
    $fn_frame->enter($fn_asm);
    ($$..-1)&1 or $fn_frame->set($fn_asm, $_) for @$args;

    my $ret = pop @body;
    $fn_frame->get($fn_asm, $_->compile($fn_frame, $fn_asm)), $fn_asm->drop
      for @body;
    $fn_frame->get($fn_asm, $ret->compile($fn_frame, $fn_asm));
    $fn_frame->exit;
    $fn_asm->endf;

    $frame->set($asm->code($fn_asm), $$sexp{var});
    $$sexp{var};
  };


=head3 C<(if cond then else)>
C<then> and C<else> are evaluated as they are in Lisp; this isn't a strict
C<if>. We bridge this gap by treating C<then> and C<else> as lightweight
functions: we use C<call> to get to either one, and each stores the continuation
into a frame slot.

We need to use frame storage for the merge continuation for two reasons:

1. The stack is small and strictly bounded, so we can't have a net element
2. If the code gets moved during a GC, we need to update the merge continuation
=cut

defspecial 'if',
  collect_bindings => sub
  {
    my ($frame, $sexp)       = @_;
    my ($cond, $then, $else) = $sexp->tail;
    $_->collect_bindings($frame) for $sexp->tail;

    my $then_ctti = $frame->ctti($then->var);
    my $else_ctti = $frame->ctti($else->var);
    warn sprintf "$sexp: then/else CTTIs differ: %s vs %s",
                 $then_ctti, $else_ctti
      unless $then_ctti eq $else_ctti;

    $frame->bind($$sexp{merge_continuation} //= gensym, 'hereptr', 1)
          ->bind($$sexp{var} //= gensym,
                 $$sexp{ctti} = $then_ctti,
                 1);
  },
  compile => sub
  {
    my ($frame, $asm, $sexp) = @_;
    my ($cond, $then, $else) = $sexp->tail;
    my $merge_var            = $$sexp{var};

    my $then_asm = phi::asm->new;
    $frame->set($then_asm, $$sexp{merge_continuation});
    $frame->get($then_asm, $then->compile($frame, $then_asm));
    $frame->get($then_asm, $$sexp{merge_continuation})->go;

    my $else_asm = phi::asm->new;
    $frame->set($else_asm, $$sexp{merge_continuation});
    $frame->get($else_asm, $else->compile($frame, $else_asm));
    $frame->get($else_asm, $$sexp{merge_continuation})->go;

    $frame->get($asm, $cond->compile($frame, $asm));
    $asm->code($then_asm)->code($else_asm)->if->call;

    $frame->set($asm, $merge_var);
    $merge_var;
  };


=head3 C<(while cond body...)>
We manage two continuations here and always return C<int(0)>. The first
continuation evaluates C<cond> and then either evaluates C<body...> and
re-invokes the first continuation, and the second continuation exits the loop
and continues the code normally. In code terms it looks like this:

  loop_continuation = code(
    frame_set(exit_continuation)
    cond
    code( body... frame_get(exit_continuation) loop_continuation go )
    frame_get(exit_continuation)
    if go )
  call loop_continuation

=cut

defspecial 'while',
  collect_bindings => sub
  {
    my ($frame, $sexp) = @_;
    $_->collect_bindings($frame) for $sexp->tail;
    $frame->bind($$sexp{loop_continuation} //= gensym, 'hereptr')
          ->bind($$sexp{exit_continuation} //= gensym, 'hereptr')
          ->bind($$sexp{var}               //= gensym, 'int', 1);
  },
  compile => sub
  {
    my ($frame, $asm, $sexp) = @_;
    my ($cond, @body) = $sexp->tail;

    my $loop_asm = phi::asm->new;
    $frame->set($loop_asm, $$sexp{loop_continuation});
    $frame->get($loop_asm, $cond->compile($frame, $loop_asm));

    my $body_asm = phi::asm->new;
    $frame->get($body_asm, $_->compile($frame, $body_asm))->drop for @body;
    $frame->get($body_asm, $$sexp{exit_continuation});
    $frame->get($body_asm, $$sexp{loop_continuation});
    $body_asm->go;

    $loop_asm->code($body_asm);
    $frame->get($loop_asm, $$sexp{exit_continuation});
    $loop_asm->if->go;

    $frame->set($asm->code($loop_asm)->call->l(0), $$sexp{var});
    $$sexp{var};
  };


=head3 C<(= var value)>
Pretty simple: reassign an already-defined quantity. This will fail if C<var>
hasn't been C<def>'d and isn't a function argument.
=cut

defspecial '=',
  collect_bindings => sub
  {
    my ($frame, $sexp) = @_;
    my (undef, $value) = $sexp->tail;
    $value->collect_bindings($frame);
  },
  compile => sub
  {
    my ($frame, $asm, $sexp) = @_;
    my ($var, $value) = $sexp->tail;
    $frame->get($asm, $value->compile($frame, $asm));
    $frame->set($asm, $var);
    $var;
  };


=head2 Inlines
phi1 does the dumb thing here and monomorphically resolves inlines based on name
alone. phi2 is much smarter about it.
=cut

sub definline($$) { inlines->{$_[0]} = $_[1] }

definline back  => sub { shift->back };

definline '+'   => sub { shift->iadd };
definline '*'   => sub { shift->imul };
definline '/'   => sub { shift->idivmod->drop };
definline '%'   => sub { shift->idivmod->sset->C(0) };
definline '-'   => sub { shift->swap->ineg->iplus };
definline '<<'  => sub { shift->ishl };
definline '>>'  => sub { shift->isar };
definline '>>>' => sub { shift->ishr };
definline '&'   => sub { shift->iand };
definline '|'   => sub { shift->ior };
definline '^'   => sub { shift->ixor };
definline '!'   => sub { shift->l(0)->l(1)->if };
definline '~'   => sub { shift->iinv };

definline $_ => eval "sub { shift->$_ }" for qw/ x16 x32 x64 /;

definline $_ => eval "sub { shift->$_ }"                   for qw/ g8 g16 g32 g64 /;
definline $_ => eval "sub { shift->sget->C(1)->swap->$_ }" for qw/ s8 s16 s32 s64 /;

definline mset => sub { shift->mset->l(0) };
definline mcpy => sub { shift->mcpy->l(0) };
definline mcmp => sub { shift->mcmp };
definline mfnd => sub { shift->mfnd };
definline unh4 => sub { shift->unh4 };


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

sub read_sexp_() { /\G\(/gc && read_space ? read_sexp_list : read_sexp_atom }

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
