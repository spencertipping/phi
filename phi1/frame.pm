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

no warnings 'portable';


=head1 Frame class generation
Frames are simple: storage-wise, we just need to know how many overall things
they store, and for GC purposes we need to know the types of those things. For
example, here's a function:

  (fn (int x ptr y)
    (def int z (g64 (iadd x y)))
    z)

Here's what the frame class will look like:

  struct
  {
    hereptr class;
    ptr     parent_frame;               # offset = 8
    ptr     lexical_parent_frame;       # offset = 16
    hereptr calling_continuation;       # offset = 24
    int     x;
    ptr     y;
    int     tmp0;                       # (iadd x y)
    int     tmp1;                       # (g64 tmp0)
    int     z;
  };


=cut

our $gensym_id = 0;
sub gensym() { sprintf "tmp%d", $gensym_id++ }

package phi::frame
{
  sub new
  {
    my $class = shift;
    bless({ size           => 0,
            vars           => {},
            lexical_parent => undef,
            finalized      => 0 }, $class)
      ->bind(class_fn       => 'hereptr')
      ->bind(parent_frame   => 'ptr')
      ->bind(lexical_parent => 'ptr')
      ->bind(cc             => 'hereptr');
  }

  sub parent { shift->{lexical_parent} }
  sub child
  {
    my $self = shift;
    my $child = ref($self)->new;
    $$child{lexical_parent} = $self;
    $child;
  }

  sub bind
  {
    my ($self, $var, $ctti, $linear) = @_;
    die "can't bind $var within a finalized frame" if $$self{finalized};
    die "can't rebind existing variable $var" if exists $$self{vars}{$var};
    $$self{vars}{$var} = { ctti   => $ctti,
                           slot   => $$self{size}++,
                           linear => $linear // 0 };
    $self;
  }

  sub ctti
  {
    my ($self, $var) = @_;
    $$self{vars}{$var}{ctti};
  }

  sub get_ignore_linear
  {
    # GC methods use this getter because it doesn't register as a
    # linear-quantity access. That is, it doesn't cause a linear quantity to be
    # unpinned.
    #
    # NB: this function doesn't support captured quantities because the GC
    # delegates to the lexical parent frame as an object rather than descending
    # and tracing its values in the same pass. Put differently, I'm being lazy
    # because I can get away with it here; technically we should support
    # captures but it doesn't matter due to the way phi1 works.
    my ($self, $asm, $var) = @_;
    exists $$self{vars}{$var} or die "can't frame-get undefined variable $var";
    $asm->fi->Lb($$self{vars}{$var}{slot} * 8)->g64;
  }

  sub get_frameonstack
  {
    my ($self, $asm, $var) = @_;
    if (exists $$self{vars}{$var})
    {
      # Any variable with a capturable name is never linear, so we can ignore
      # that case here.
      $asm->l($$self{vars}{$var}{slot} * 8)->iadd->g64;
    }
    elsif (defined $self->parent)
    {
      $asm->l(16)->iadd->g64;                                 # f -> parent
      $self->parent->get_frameonstack($asm, $var);
    }
    else
    {
      die "can't frame->get unbound variable $var";
    }
  }

  sub get
  {
    my ($self, $asm, $var) = @_;

    if (exists $$self{vars}{$var})
    {
      # Set linear variables to zero after a single access. This minimizes the
      # GC live set.
      $$self{vars}{$var}{linear}
        ? $asm->fi->Lb($$self{vars}{$var}{slot} * 8)          # &x
              ->dup->g64->swap                                # x &x
              ->l(0)->swap->s64                               # x
        : $asm->fi->Lb($$self{vars}{$var}{slot} * 8)->g64;
    }
    elsif (defined $self->parent)
    {
      $asm->fi->Lb(16)->g64;                                  # parent
      $self->parent->get_frameonstack($asm, $var);
    }
    else
    {
      die "can't frame->get unbound variable $var";
    }
  }

  sub set
  {
    # NB: technically we should support set() for captured variables, but phi1
    # doesn't rely on that feature so I haven't implemented it.
    my ($self, $asm, $var) = @_;
    exists $$self{vars}{$var} or die "can't frame-set undefined variable $var";
    $asm->fi->Lb($$self{vars}{$var}{slot} * 8)->s64;
  }

  sub frame_class
  {
    # TODO: we need support for GC-related methods
    0xf4aec1a55;
  }

  sub enter
  {
    my ($self, $asm) = @_;

    # We can't bind any new variables once we commit to a frame size.
    $$self{finalized} = 1;

    $asm->fi->Lb(-$$self{size} * 8)                 # f'
        ->l($self->frame_class)->sget->C(1)->s64    # f' [.class_fn=]
        ->fi->Lb(0)
        ->fi->Lb(-($$self{size} - 1) * 8)->s64      # f' [.parent_frame=]
        ->sf                                        # [f=f']
        ->fi->Lb(24)->s64                           # [.cc=]
        ->fi->Lb(16)->s64;                          # [.lexical_parent=]

    # Initialize the variable to zero, regardless of type. The first four slots
    # are already initialized, so we skip those.
    $asm->l(0)->fi->Lb($_ * 8)->s64 for 4 .. $$self{size} - 1;
    $asm;
  }

  sub exit
  {
    my ($self, $asm) = @_;
    $asm->fi->Lb(24)->g64                           # cc
        ->fi->Lb(8)->g64->sf                        # cc [f=parent]
        ->go;                                       # ->cc
  }
}


1;
