#!/usr/bin/env perl

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

package phi::asm;

use strict;
use warnings;

use phi::use 'phi::asm' => sub
{
  return unless @_;
  my ($name, $asm) = @_;
  $name => phi::const $asm->named($name);
};


=head2 Object assembler
We could write the phi boot image in assembly language. The big advantage to
doing it in Perl is that we get better automation and finer control over the
exact machine instructions that get emitted. Both of these things matter a lot.

At a high level, here's what we need from an assembler interface:

1. Hex literal bytes
2. Octal literal bytes (mostly for ModR/M encoding)
3. Namespaced labels
4. Absolute positioning/addressing
5. C<pack> references to constants, relative labels, or absolute links

Everything ends up with an absolute address, so the linker can use the standard
two-pass design: allocate space first, then patch in the correct locations. We
don't have any addresses that change size, e.g. C<jmp label> in assembly; all of
the sizes are hard-coded up front and we do a quick overflow check to make sure
C<jmp short> (and other small-value) instructions are within range.


=head3 Authoring things
What we really need is something that behaves like a string but remains valid
(in the binary-linkage sense) even if we don't know the heap location of
everything up front. We should be able to insert C<pack> placeholders whose
values will later be computed, and we should also be able to easily grab a
pointer to any location within a string.

I think we want something like this:

  my $fn   = asm->c3;
  my $code = asm->e8pl($fn - right)->c3;

We first add everything to the heap; then once the addresses are set, the
objects can go back and patch themselves.
=cut


sub phi::left();
sub phi::right();
sub phi::asm(;$);
sub phi::l($);

sub phi::location_of
{
  my ($x, $asm) = @_;
  my $l = ref $x ? $x->location($asm) : $x;
  die "$x has no defined location" unless defined $l;
  $l;
}


package phi::asm
{
  # Shorthand: asm->... instead of phi::asm->new->...
  sub phi::asm(;$) { phi::asm::->new(shift) }

  our %all_located;                     # DEBUG_EMIT_SYMBOLS

  use overload qw/ 0+ ptr "" linked << inline /;

  sub new
  {
    my ($class, $name) = @_;
    bless { template => [],
            args     => [],
            labels   => {},
            name     => $name,
            location => undef }, $class;
  }

  sub named      { $_[0]->{name} = $_[1]; shift }
  sub is_located { defined shift->{location} }
  sub size       { length shift->unlinked }
  sub ptr        { phi::asm_ptr->new(shift) }
  sub rptr       { phi::asm_ptr->new($_[0], $_[0]->size, 1) }

  sub from
  {
    my ($self, $label) = @_;
    substr $self->linked, phi::location_of $self->resolve($label) - $self;
  }

  sub locate
  {
    my ($self, $l) = @_;
    die $self->name . " is already located at $$self{location} (requested $l)"
      if defined $$self{location} && $$self{location} != $l;
    $$self{location} = $l;
    $all_located{$self->name} = $self   # DEBUG_EMIT_SYMBOLS
      if defined $$self{name};
    $self;
  }

  sub name
  {
    my $self = shift;
    $$self{name} // "anonymous[" . join(" ", @{$$self{template}}) . "]";
  }

  sub location
  {
    my $self = shift;
    $$self{location} // die "unallocated asm object " . $self->name;
  }

  sub dependencies { grep ref, map @$_, @{shift->{args}} }
  sub unlinked     { shift->compile(sub { 0 }) }
  sub linked
  {
    my $self = shift;
    die "cannot link unallocated asm object " . $self->name
      unless $self->is_located or !$self->dependencies;
    $self->compile(sub { shift->location($self) });
  }

  sub link_at
  {
    # Link in this object and all unallocated dependencies (transitively),
    # returning a flat memory allocation.
    my ($self, $location) = @_;
    die "link_at: moving already-located object " . $self->name . " from "
      . "$$self{location} to $location"
      if $self->is_located && $self->location != $location;

    $self->locate($location);
    my $asm = phi::asm->locate($location);
    $location += $self->size;

    # Record all labels into the location list
    if (phi::DEBUG_EMIT_SYMBOLS and defined $$self{name})
    {
      $all_located{$self->name . "/$_"} = $self->resolve($_)
        for keys %{$$self{labels}};
    }

    my @linked;

    for ($self->dependencies)
    {
      next if $_->is_located;
      push @linked, $_ = $_->link_at($location);
      $location += $_->size;
    }

    $asm->lit($_->linked) for $self, @linked;
    $asm;
  }

  sub safe_pack
  {
    # Identical to pack(), but verifies that none of the numeric args has
    # overflowed. We detect this by unpacking to make sure we reconstruct the
    # original list.
    my ($template, @xs) = @_;
    my $packed = pack $template, @xs;
    my $xs = join", ", @xs;
    my $ys = join", ", unpack $template, $packed;
    lc $xs eq lc $ys or
      die "phi::asm: inconsistent reconstruction for $template: [$xs] vs [$ys]";

    $packed;
  }

  sub compile
  {
    my ($self, $resolver) = @_;
    join"", map safe_pack($$self{template}[$_], map ref ? $resolver->($_) : $_,
                                                    @{$$self{args}[$_]}),
                0..$#{$$self{template}};
  }

  sub rewrite_left_right { shift }
  sub rewrite_here
  {
    my $self  = shift;
    my $left  = shift;
    my $right = $self->rptr;
    map ref ? $_->rewrite_left_right($left, $right) : $_, @_;
  }

  sub append
  {
    my $self     = shift;
    my $template = shift;
    my $left     = $self->rptr;

    # First, stage the arguments so we can calculate the new unlinked size.
    push @{$$self{template}}, $template;
    push @{$$self{args}},     [@_];

    my @args = $self->rewrite_here($left, @_);

    # Now we have revised offsets for delta pointers, so replace the args we
    # just added with the rewritten ones.
    #
    # NB: this means you can't reliably refer to the RHS of things like
    # BER-encoeed integers, but that kinda makes sense.
    $$self{args}[-1] = \@args;
    $self;
  }

  sub hex { shift->append("H*", join"", @_) }
  sub oct { shift->append("C",  CORE::oct shift) }
  sub lit { shift->append("a*", shift) }

  sub inline
  {
    my ($self, $asm) = @_;
    $asm = $asm->link_at($self->rptr->location) if $self->is_located;
    $self->append("a*", $asm->linked);
  }

  sub label
  {
    my ($self, $name) = @_;
    die "redefining label $name" if exists $$self{labels}{$name};
    $$self{labels}{$name} = $self->rptr;
    $self;
  }

  sub resolve
  {
    my ($self, $label) = @_;
    $$self{labels}{$label} // die $self->name . ": undefined label $label";
  }

  # NB: we need to define this to prevent it from falling into AUTOLOAD
  sub DESTROY {}

  # Syntactic completion for a few types of methods depending on their pattern
  our $AUTOLOAD;
  sub AUTOLOAD
  {
    my $self   = shift;
    my $method = $AUTOLOAD =~ s/.*:://r;

    while (length $method)
    {
      next if $method =~ s/^_+//;

      $self->hex($1),        next if $method =~ s/^x?((?:[0-9a-fA-F]{2})+)//;
      $self->oct($1),        next if $method =~ s/^o([0-3][0-7]{2})//;
      $self->lit($method),   last if $method =~ s/^q//;
      $self->label($method), last if $method =~ s/^l//;

      # Direct pack templates: force little-endian on all native integer types,
      # use R as a shorthand for the / repeat specifier.
      $self->append($method =~ s/([slqiSLQI])/($1\<)/gr
                            =~ s/R/\//gr
                            =~ s/_//gr,
                    @_), last
        if $method =~ s/^p//;

      die "phi::asm: failed to parse method snippet starting with $method "
        . "(original call was \"$AUTOLOAD\")";
    }
    $self;
  }
}


package phi::asm_ptr
{
  BEGIN { ++$INC{'phi/asm_ptr.pm'} }

  use Scalar::Util qw/refaddr/;
  use overload qw| +  plus
                   -  minus
                   *  times
                   /  over
                   0+ location |;

  sub new
  {
    my ($class, $base, $delta, $scale) = @_;
    bless { base  => $base,
            delta => $delta // 0,
            scale => $scale // 1 }, $class;
  }

  sub size  { 0 }

  sub plus  { phi::asm_ptr->new(shift, shift,  1) }
  sub minus { phi::asm_ptr->new(shift, shift, -1) }
  sub times { phi::asm_ptr->new(0, shift,   shift) }
  sub over  { phi::asm_ptr->new(0, shift, 1/shift) }

  sub rewrite
  {
    my ($x, $l, $r) = @_;
    return $x unless ref $x;
      refaddr $x eq refaddr phi::left  ? $l
    : refaddr $x eq refaddr phi::right ? $r
    : $x;
  }

  sub rewrite_left_right
  {
    my ($self, $l, $r) = @_;
    phi::asm_ptr->new(rewrite($$self{base},  $l, $r),
                      rewrite($$self{delta}, $l, $r),
                      $$self{scale});
  }

  sub is_located   { not grep !$_->is_located, shift->dependencies }
  sub dependencies { grep ref, @{+shift}{'base', 'delta'} }
  sub link_at
  {
    my ($self, $l) = @_;
    my $asm = phi::asm->locate($l);
    $asm->inline($$self{base})  if ref $$self{base};
    $asm->inline($$self{delta}) if ref $$self{delta};
    $asm;
  }

  sub location
  {
    my ($self, $asm) = @_;
    phi::location_of($$self{base}, $asm)
      + int $$self{scale} * phi::location_of($$self{delta}, $asm);
  }
}


package phi::asm_label
{
  BEGIN { ++$INC{'phi/asm_label.pm'} }

  # Shorthand: l"name" to refer to a label
  sub phi::l($) { phi::asm_label->new(shift) }

  use parent 'phi::asm_ptr';

  sub new
  {
    my ($class, $name) = @_;
    bless \$name, $class;
  }

  sub rewrite_left_right { shift }

  sub link_at      { phi::asm }
  sub dependencies { () }
  sub is_located   { 1 }
  sub location
  {
    my ($self, $asm) = @_;
    phi::location_of $asm->resolve($$self), $asm;
  }
}


# Magic references that will be rewritten (NB: bless into the class only once
# the class is defined; otherwise we may run into problems with operator
# overloading due to a perl magic-caching bug -- I forget which version fixed
# this).
#
# More specifically, "left" and "right" refer to the bounds of the current pack
# template expression. These are used to emit relative jumps, for example.
package phi
{
  use constant left  => bless \(my $x = "left"),  'phi::asm_ptr';
  use constant right => bless \(my $x = "right"), 'phi::asm_ptr';
}


1;
